const tapes = [
".HH.H.H.HH.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.HH.H.H.HH.H.HH.H.H.H",
"H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.HH.H.H.H.",
".H.H.H.H.H.HH.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.H",
"H.H.H.H.HH.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.H",
".H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.HH.H.H.HH.H.H.H.HH.H.H.",
"H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH.H.H.H.H.HH",
"H.HH.H.H.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.H.HH.H.H.H.H.H.H.H.H.H.HH.H.H.H.H.",
"HH.HH.H.HH.H.HH.HH.H.HH.H.HH.HH.H.HH.H.HH.H.HH.HH.H.HH.H.HH.HH.H.HH.H.HH.HH.H.HH.H.HH.HH.H.HH.H.HH.HH.H.HH.H.HH.HH.H.HH.",
"H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.HH.H.H.HH.H.H.H",
".H.H.H.H.H.HH.H.H.H.H.H.H.HH.H.H.H.H.H.H.HH.H.H.H.H.H.H.HH.H.H.H.H.H.H.HH.H.H.H.H.H.H.HH.H.H.H.H.H.H.HH.H.H.H.H.H.H.HH.H",
];
const norm = (x:number)=>((x%1)+1)%1;
for (const [k,s] of tapes.entries()) {
  const t = s.split("").map(c=>c==="H"?1:0), n=t.length;
  let best:any=null;
  for(let A=1;A<20000;A++){const a=A/20000;
    // for this a, derive feasible b interval by scanning candidate b quickly
    for(let B=0;B<400;B++){const b=B/400;
      let bad=0; for(let i=0;i<n;i++){ if(((norm(i*a+b)<a)?0:1)!==t[i]) {bad++; if(best&&bad>best.bad)break;} }
      if(!best||bad<best.bad)best={a,b,bad};}
    if(best&&best.bad===0)break;
  }
  const dens = t.filter(x=>!x).length/n;
  console.log(`tape ${k+1}: n=${n} openDensity=${dens.toFixed(4)}  bestFit a=${best.a.toFixed(5)} b=${best.b.toFixed(3)} errors=${best.bad}  |a-density|=${Math.abs(best.a-dens).toFixed(4)}`);
}
