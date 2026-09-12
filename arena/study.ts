const t = Deno.args[0].split("").map(c=>c==="H"?1:0);
const n=t.length;
const opens=[...t.keys()].filter(i=>!t[i]);
console.log("len",n,"opens",opens.length,"density(open)",(opens.length/n).toFixed(4));
const gaps=opens.slice(1).map((v,i)=>v-opens[i]);
console.log("open gaps:",gaps.join(""));
// Sturmian / rotation test: open at i iff frac(i*a+b) < a  (mechanical word)
let best:any=null;
for(let A=1;A<=2000;A++){const a=A/2000;
  for(let B=0;B<200;B++){const b=B/200;
    let bad=0; for(let i=0;i<n;i++){const f=(i*a+b)%1; const pred=f<a?0:1; if(pred!==t[i])bad++;}
    if(!best||bad<best.bad)best={a,b,bad};}}
console.log("best rotation fit:",best);
// run-length of H
const runs:string[]=[];let c=t[0],k=0;for(const x of t){if(x===c)k++;else{runs.push((c?"H":".")+k);c=x;k=1;}}runs.push((c?"H":".")+k);
console.log("runs:",runs.join(" "));
