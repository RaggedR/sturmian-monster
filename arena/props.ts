const tapes=(await Deno.readTextFile("tapes.txt")).trim().split("\n");
for(const [k,s] of tapes.entries()){
  const t=s.split("").map(c=>c==="H"?1:0), n=t.length;
  const op=[...t.keys()].filter(i=>!t[i]); const gaps=op.slice(1).map((v,i)=>v-op[i]);
  const gs=[...new Set(gaps)].sort((a,b)=>a-b);
  // shortest exact period covering whole tape
  let per=n; for(let p=1;p<n;p++){let ok=true;for(let i=p;i<n;i++)if(t[i]!==t[i-p]){ok=false;break;} if(ok){per=p;break;}}
  // balance: max |#H in window w - min| over all w
  let bal=0; for(let w=1;w<=n;w++){let mn=1e9,mx=-1;for(let i=0;i+w<=n;i++){const c=t.slice(i,i+w).reduce((a,b)=>a+b,0);mn=Math.min(mn,c);mx=Math.max(mx,c);} bal=Math.max(bal,mx-mn);}
  // distinct factors of length m (Sturmian => exactly m+1)
  const fac=(m:number)=>new Set(Array.from({length:n-m+1},(_,i)=>t.slice(i,i+m).join(""))).size;
  console.log(`tape ${k+1}: gaps=${gs.join("/")} shortestPeriod=${per} maxImbalance=${bal} factors(3..6)=${[3,4,5,6].map(fac).join(",")}`);
}
