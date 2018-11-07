for(i=0;i<r.length;i+=2)s=s.split(r[i]).join(r[i+1])
w='';for(i=0;i<s.length;){c=s[i++];d=c.charCodeAt(0)-48;if(d>=0&&d<=9){console.log(w);w=w.substr(0,d);}else w+=c}
