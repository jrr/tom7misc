
let testRandTo = () => {
  for (let len of [1, 3, 7, 10]) {
    let a = [];
    for (let i = 0; i < len; i++)
      a.push(0);

    let rc = new ArcFour("test" + len);

    for (let i = 0; i < 50000; i++) {
      let x = rc.randTo32(len);
      if (x < 0 || x >= len) throw 'out of range';
      a[x]++;
    }

    for (let i = 0; i < len; i++) {
      console.log(i + ': ' + a[i]);
    }
    console.log('---');
  }
};

testRandTo();
