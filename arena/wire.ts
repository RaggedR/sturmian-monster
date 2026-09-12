// wire.ts — one JSON document in, one JSON document out, over a socket.
// Deliberately thinner than stalin's: there is no tower here, so no depth.

const enc = new TextEncoder();
export const isRecord = (u: unknown): u is Record<string, unknown> =>
  typeof u === "object" && u !== null && !Array.isArray(u);

export function trace(who: string, what: string) {
  Deno.stderr.writeSync(enc.encode(`\x1b[2m  ${who.padEnd(8)} ${what}\x1b[0m\n`));
}

export function serve(
  port: number, who: string,
  handle: (msg: Record<string, unknown>) => unknown | Promise<unknown>,
) {
  Deno.serve({ port, onListen: () => trace(who, `listening on :${port}`) }, async (req) => {
    try {
      const body = await req.json();
      if (!isRecord(body)) throw new Error("body is not an object");
      const reply = await handle(body);
      return new Response(JSON.stringify(reply), {
        headers: { "content-type": "application/json" },
      });
    } catch (e) {
      return new Response(JSON.stringify({ tag: "Error", error: String(e) }), { status: 400 });
    }
  });
}

export async function ask(port: number, msg: unknown): Promise<Record<string, unknown>> {
  const res = await fetch(`http://localhost:${port}`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(msg),
  });
  const text = await res.text();
  if (!res.ok) throw new Error(`:${port} answered ${res.status} — ${text}`);
  const v = JSON.parse(text);
  if (!isRecord(v)) throw new Error(`:${port} did not answer with an object`);
  return v;
}

/** The type is a promise; the decoder is the audit. */
export function decodeJson(text: string): unknown {
  const fenced = text.match(/```(?:json)?\s*([\s\S]*?)```/);
  const body = (fenced ? fenced[1] : text).trim();
  const start = body.search(/[{[]/);
  return JSON.parse(start >= 0 ? body.slice(start) : body);
}
