import { fit, predict } from "./engine.ts";
const tapes = (await Deno.readTextFile("tapes.txt")).trim().split("\n");
for (const seed of [6,8,10,12,16,20]) {
  let w=0, tot=0, beats=0, wh=0;
  for (const s of tapes) {
    const T = s.split("").map(c=>c==="H"?1:0);
    let hp=20,mhp=60; const hist:number[]=[]; let i=0;
    for (; i<T.length && hp>0 && mhp>0; i++) {
      let act="block";
      if(i>=seed && predict(fit(hist), i)==="OPEN") act="attack";
      hist.push(T[i]);
      if(act==="attack"){ T[i]===0 ? mhp-=2 : (hp-=5, wh++); }
    }
    if(mhp<=0){w++; beats+=i;} tot++;
  }
  console.log(`seed=${seed}: wins ${w}/${tot}  whiffs=${wh}  avg beats to kill=${(beats/Math.max(w,1)).toFixed(1)}`);
}
