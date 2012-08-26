/* events that can happen on the map. */
var events = {
    treestart: { x1: 3994, y1: 3463,
		 x2: 4198, y2: 3589,
		 scx: 3900, scy: 3280 },
    lifeguard: { x1: 6266, y1: 3977, 
		 x2: 6402, y2: 4055,
		 scx: 6187, scy: 3750 },
    shell: { x1: 9323, y1: 7540,
	     x2: 9541, y2: 7696,
	     scx: 9203, scy: 7400 },
    miners: { x1: 8008, y1: 6812,
	      x2: 8242, y2: 6993,
	      scx: 8164, scy: 6900 },
    foreman: { x1: 3899, y1: 6474,
	       x2: 4108, y2: 6612,
	       scx: 3847, scy: 6295,
	       // self-regulated
	       permanent: true },
    charlie: { x1: 8138, y1: 3820,
	       x2: 8293, y2: 3896,
	       scx: 7975, scy: 3645 },
    bird: { x1: 2969, y1: 3500,
	    x2: 3146, y2: 3614,
	    scx: 2900, scy: 3300 },
    mines: { x1: 4200, y1: 4000,
	     x2: 4318, y2: 4111,
	     scx: 3938, scy: 3830 },
};

var stuff = {
    // These will get a bm field at runtime with
    // the list of loaded frames from p, scaled 2x.
    // also a field mc to store a movie clip,
    // and a unique index idx

    mom: { x: 4050, y: 3533, p: ['mom'] },
    dad: { x: 4134, y: 3532, p: ['dad'] },
    shell: { x: 9363, y: 7551, p: ['shell1'] },
    tminer1: { x: 8362, y: 7087, p: ['miner'] },
    tminer2: { x: 8499, y: 7080, p: ['miner'] },
    foreman: { x: 4004, y: 6548, p: ['miner'] },
    // exact!
    bird: { x: 2997, y: 3523, p: ['bird'] },
    charlie: { x: 8217, y: 3855, p: ['charlie'] }
};
