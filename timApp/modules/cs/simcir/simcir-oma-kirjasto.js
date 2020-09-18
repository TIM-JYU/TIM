// SimcirJS - oma kirjasto
// Komponentit
//  2to1Mux
//  4to1Mux
//  2to1Mux4
//  4to1Mux4
//  8to1Mux4
//  1to2DeMux
//  1to4DeMux
//  1to8DeMux
//  Bit
//  4BitReg
//  8BitReg
//  4BitRam8
//  Multiplier

// 2to1Mux Toiminta:
// Sel Out
//  0	A
//  1	B

import simcir from "./simcir";

simcir.registerDevice('2to1Mux',
{
  "width":380,
  "height":200,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"Toggle","id":"dev0","x":64,"y":8,"label":"Toggle"},
    {"type":"Toggle","id":"dev1","x":64,"y":72,"label":"Toggle"},
    {"type":"In","id":"dev2","x":120,"y":8,"label":"A"},
    {"type":"Toggle","id":"dev3","x":64,"y":136,"label":"Toggle"},
    {"type":"DC","id":"dev4","x":8,"y":72,"label":"DC"},
    {"type":"Out","id":"dev5","x":320,"y":72,"label":"Out"},
    {"type":"OR","id":"dev6","x":272,"y":72,"label":"OR"},
    {"type":"AND","id":"dev7","x":224,"y":48,"label":"AND"},
    {"type":"AND","id":"dev8","x":224,"y":96,"label":"AND"},
    {"type":"In","id":"dev9","x":120,"y":72,"label":"Sel"},
    {"type":"In","id":"dev10","x":120,"y":136,"label":"B"},
    {"type":"NOT","id":"dev11","x":168,"y":56,"label":"NOT"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev4.out0"},
    {"from":"dev1.in0","to":"dev4.out0"},
    {"from":"dev2.in0","to":"dev0.out0"},
    {"from":"dev3.in0","to":"dev4.out0"},
    {"from":"dev5.in0","to":"dev6.out0"},
    {"from":"dev6.in0","to":"dev7.out0"},
    {"from":"dev6.in1","to":"dev8.out0"},
    {"from":"dev7.in0","to":"dev2.out0"},
    {"from":"dev7.in1","to":"dev11.out0"},
    {"from":"dev8.in0","to":"dev9.out0"},
    {"from":"dev8.in1","to":"dev10.out0"},
    {"from":"dev9.in0","to":"dev1.out0"},
    {"from":"dev10.in0","to":"dev3.out0"},
    {"from":"dev11.in0","to":"dev9.out0"}
  ]
}
);

// 4to1Mux Toiminta:
// Sel1 Sel0 Out
//   0    0	  A
//   0	  1	  B
//   1	  0	  C
//   1	  1	  D
simcir.registerDevice('4to1Mux',
{
  "width":320,
  "height":300,
  "showToolbox":false,
  "toolbox":[
  ],
"devices":[
    {"type":"In","id":"dev0","x":64,"y":8,"label":"A"},
    {"type":"In","id":"dev1","x":64,"y":56,"label":"B"},
    {"type":"In","id":"dev2","x":64,"y":104,"label":"C"},
    {"type":"In","id":"dev3","x":64,"y":152,"label":"D"},
    {"type":"In","id":"dev4","x":64,"y":200,"label":"Sel0"},
    {"type":"DC","id":"dev5","x":8,"y":120,"label":"DC"},
    {"type":"2to1Mux","id":"dev6","x":120,"y":112,"label":"2to1Mux"},
    {"type":"2to1Mux","id":"dev7","x":200,"y":72,"label":"2to1Mux"},
    {"type":"2to1Mux","id":"dev8","x":120,"y":24,"label":"2to1Mux"},
    {"type":"Out","id":"dev9","x":280,"y":80,"label":"Out"},
    {"type":"In","id":"dev10","x":64,"y":248,"label":"Sel1"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev5.out0"},
    {"from":"dev1.in0","to":"dev5.out0"},
    {"from":"dev2.in0","to":"dev5.out0"},
    {"from":"dev3.in0","to":"dev5.out0"},
    {"from":"dev4.in0","to":"dev5.out0"},
    {"from":"dev6.in0","to":"dev2.out0"},
    {"from":"dev6.in1","to":"dev4.out0"},
    {"from":"dev6.in2","to":"dev3.out0"},
    {"from":"dev7.in0","to":"dev8.out0"},
    {"from":"dev7.in1","to":"dev10.out0"},
    {"from":"dev7.in2","to":"dev6.out0"},
    {"from":"dev8.in0","to":"dev0.out0"},
    {"from":"dev8.in1","to":"dev4.out0"},
    {"from":"dev8.in2","to":"dev1.out0"},
    {"from":"dev9.in0","to":"dev7.out0"},
    {"from":"dev10.in0","to":"dev5.out0"}
  ]
 }
);

// 2to1Mux4 Toiminta, 4-bittinen 2 input mux
// Sel Out
//  0	A[4]
//  1	B[4]
simcir.registerDevice('2to1Mux4',
{
  "width":300,
  "height":280,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"2to1Mux","id":"dev0","x":128,"y":72,"label":"2to1Mux"},
    {"type":"2to1Mux","id":"dev1","x":128,"y":136,"label":"2to1Mux"},
    {"type":"2to1Mux","id":"dev2","x":128,"y":200,"label":"2to1Mux"},
    {"type":"2to1Mux","id":"dev3","x":128,"y":8,"label":"2to1Mux"},
    {"type":"BusIn","id":"dev4","x":72,"y":8,"label":"BusIn","numOutputs":4},
    {"type":"In","id":"dev5","x":16,"y":16,"label":"A"},
    {"type":"In","id":"dev6","x":16,"y":88,"label":"B"},
    {"type":"BusIn","id":"dev7","x":72,"y":80,"label":"BusIn","numOutputs":4},
    {"type":"BusOut","id":"dev8","x":200,"y":112,"label":"BusOut","numInputs":4},
    {"type":"Out","id":"dev9","x":256,"y":112,"label":"Out"},
    {"type":"In","id":"dev10","x":24,"y":144,"label":"Sel"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev4.out1"},
    {"from":"dev0.in1","to":"dev10.out0"},
    {"from":"dev0.in2","to":"dev7.out1"},
    {"from":"dev1.in0","to":"dev4.out2"},
    {"from":"dev1.in1","to":"dev10.out0"},
    {"from":"dev1.in2","to":"dev7.out2"},
    {"from":"dev2.in0","to":"dev4.out3"},
    {"from":"dev2.in1","to":"dev10.out0"},
    {"from":"dev2.in2","to":"dev7.out3"},
    {"from":"dev3.in0","to":"dev4.out0"},
    {"from":"dev3.in1","to":"dev10.out0"},
    {"from":"dev3.in2","to":"dev7.out0"},
    {"from":"dev4.in0","to":"dev5.out0"},
    {"from":"dev7.in0","to":"dev6.out0"},
    {"from":"dev8.in0","to":"dev3.out0"},
    {"from":"dev8.in1","to":"dev0.out0"},
    {"from":"dev8.in2","to":"dev1.out0"},
    {"from":"dev8.in3","to":"dev2.out0"},
    {"from":"dev9.in0","to":"dev8.out0"}
  ]
}
);

// 4to1Mux4 Toiminta, 4-bittinen 4 input mux
// Sel[1] Sel[0] Out
//   0      0     A
//   0      1     B
//   1      0     C
//   1      1     D
simcir.registerDevice('4to1Mux4',
{
  "width":310,
  "height":280,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"In","id":"dev0","x":32,"y":24,"label":"A"},
    {"type":"In","id":"dev1","x":32,"y":72,"label":"B"},
    {"type":"In","id":"dev2","x":32,"y":120,"label":"C"},
    {"type":"In","id":"dev3","x":32,"y":168,"label":"D"},
    {"type":"2to1Mux4","id":"dev4","x":96,"y":32,"label":"2to1Mux4"},
    {"type":"2to1Mux4","id":"dev5","x":96,"y":128,"label":"2to1Mux4"},
    {"type":"2to1Mux4","id":"dev6","x":184,"y":88,"label":"2to1Mux4"},
    {"type":"Out","id":"dev7","x":272,"y":96,"label":"Out"},
    {"type":"In","id":"dev8","x":32,"y":216,"label":"Sel0"},
    {"type":"In","id":"dev9","x":96,"y":216,"label":"Sel1"}
  ],
  "connectors":[
    {"from":"dev4.in0","to":"dev0.out0"},
    {"from":"dev4.in1","to":"dev1.out0"},
    {"from":"dev4.in2","to":"dev8.out0"},
    {"from":"dev5.in0","to":"dev2.out0"},
    {"from":"dev5.in1","to":"dev3.out0"},
    {"from":"dev5.in2","to":"dev8.out0"},
    {"from":"dev6.in0","to":"dev4.out0"},
    {"from":"dev6.in1","to":"dev5.out0"},
    {"from":"dev6.in2","to":"dev9.out0"},
    {"from":"dev7.in0","to":"dev6.out0"}
  ]
}
);

// 8to1Mux4 Toiminta, 4-bittinen 8 input mux
// Sel[2] Sel[1] Sel[0] Out
//   0      0      0     A
//   0      0      1     B
//   0      1      0     C
//   0      1      1     D
//   1      0      0     E
//   1      0      1     F
//   1      1      0     G
//   1      1      1     H
simcir.registerDevice('8to1Mux4',
{
  "width":310,
  "height":280,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"2to1Mux4","id":"dev0","x":264,"y":64,"label":"2to1Mux4"},
    {"type":"Out","id":"dev1","x":344,"y":72,"label":"Out"},
    {"type":"4to1Mux4","id":"dev2","x":168,"y":24,"label":"4to1Mux4"},
    {"type":"4to1Mux4","id":"dev3","x":168,"y":136,"label":"4to1Mux4"},
    {"type":"In","id":"dev4","x":8,"y":8,"label":"A"},
    {"type":"In","id":"dev5","x":8,"y":56,"label":"B"},
    {"type":"In","id":"dev6","x":8,"y":104,"label":"C"},
    {"type":"In","id":"dev7","x":8,"y":152,"label":"D"},
    {"type":"In","id":"dev8","x":48,"y":32,"label":"E"},
    {"type":"In","id":"dev9","x":48,"y":80,"label":"F"},
    {"type":"In","id":"dev10","x":48,"y":128,"label":"G"},
    {"type":"In","id":"dev11","x":48,"y":176,"label":"H"},
    {"type":"In","id":"dev12","x":72,"y":216,"label":"Sel0"},
    {"type":"In","id":"dev13","x":104,"y":248,"label":"Sel1"},
    {"type":"In","id":"dev14","x":136,"y":280,"label":"Sel2"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev2.out0"},
    {"from":"dev0.in1","to":"dev3.out0"},
    {"from":"dev0.in2","to":"dev14.out0"},
    {"from":"dev1.in0","to":"dev0.out0"},
    {"from":"dev2.in0","to":"dev4.out0"},
    {"from":"dev2.in1","to":"dev5.out0"},
    {"from":"dev2.in2","to":"dev6.out0"},
    {"from":"dev2.in3","to":"dev7.out0"},
    {"from":"dev2.in4","to":"dev12.out0"},
    {"from":"dev2.in5","to":"dev13.out0"},
    {"from":"dev3.in0","to":"dev8.out0"},
    {"from":"dev3.in1","to":"dev9.out0"},
    {"from":"dev3.in2","to":"dev10.out0"},
    {"from":"dev3.in3","to":"dev11.out0"},
    {"from":"dev3.in4","to":"dev12.out0"},
    {"from":"dev3.in5","to":"dev13.out0"}
  ]
}
);


// 1to2DeMux Toiminta:
// Sel	A	B
//  0	In	0
//  1	0	In
simcir.registerDevice('1to2DeMux',
{
  "width":300,
  "height":120,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"Toggle","id":"dev0","x":64,"y":8,"label":"Toggle"},
    {"type":"Toggle","id":"dev1","x":64,"y":64,"label":"Toggle"},
    {"type":"DC","id":"dev2","x":8,"y":32,"label":"DC"},
    {"type":"In","id":"dev3","x":112,"y":64,"label":"Sel"},
    {"type":"In","id":"dev4","x":112,"y":8,"label":"In"},
    {"type":"AND","id":"dev5","x":208,"y":8,"label":"AND"},
    {"type":"Out","id":"dev6","x":256,"y":8,"label":"A"},
    {"type":"Out","id":"dev7","x":256,"y":64,"label":"B"},
    {"type":"AND","id":"dev8","x":208,"y":64,"label":"AND"},
    {"type":"NOT","id":"dev9","x":160,"y":32,"label":"NOT"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev2.out0"},
    {"from":"dev1.in0","to":"dev2.out0"},
    {"from":"dev3.in0","to":"dev1.out0"},
    {"from":"dev4.in0","to":"dev0.out0"},
    {"from":"dev5.in0","to":"dev4.out0"},
    {"from":"dev5.in1","to":"dev9.out0"},
    {"from":"dev6.in0","to":"dev5.out0"},
    {"from":"dev7.in0","to":"dev8.out0"},
    {"from":"dev8.in0","to":"dev4.out0"},
    {"from":"dev8.in1","to":"dev3.out0"},
    {"from":"dev9.in0","to":"dev3.out0"}
  ]
 }
);

// 1to4DeMux Toiminta:
// Sel1	Sel0 A	 B   C   D
//   0	  0	In	 0	 0	 0
//   0	  1	 0	In	 0	 0
//   1	  0	 0	 0	In	 0
//   1	  1	 0	 0	 0	In
simcir.registerDevice('1to4DeMux',
{
  "width":380,
  "height":200,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"In","id":"dev0","x":104,"y":32,"label":"In"},
    {"type":"Toggle","id":"dev1","x":56,"y":32,"label":"Toggle"},
    {"type":"In","id":"dev2","x":104,"y":80,"label":"Sel0"},
    {"type":"In","id":"dev3","x":104,"y":128,"label":"Sel1"},
    {"type":"DC","id":"dev4","x":8,"y":80,"label":"DC"},
    {"type":"Toggle","id":"dev5","x":56,"y":80,"label":"Toggle"},
    {"type":"Toggle","id":"dev6","x":56,"y":128,"label":"Toggle"},
    {"type":"Out","id":"dev7","x":336,"y":8,"label":"A"},
    {"type":"Out","id":"dev8","x":336,"y":56,"label":"B"},
    {"type":"Out","id":"dev9","x":336,"y":104,"label":"C"},
    {"type":"Out","id":"dev10","x":336,"y":152,"label":"D"},
    {"type":"1to2DeMux","id":"dev11","x":256,"y":32,"label":"1to2DeMux"},
    {"type":"1to2DeMux","id":"dev12","x":256,"y":128,"label":"1to2DeMux"},
    {"type":"1to2DeMux","id":"dev13","x":168,"y":80,"label":"1to2DeMux"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev1.out0"},
    {"from":"dev1.in0","to":"dev4.out0"},
    {"from":"dev2.in0","to":"dev5.out0"},
    {"from":"dev3.in0","to":"dev6.out0"},
    {"from":"dev5.in0","to":"dev4.out0"},
    {"from":"dev6.in0","to":"dev4.out0"},
    {"from":"dev7.in0","to":"dev11.out0"},
    {"from":"dev8.in0","to":"dev11.out1"},
    {"from":"dev9.in0","to":"dev12.out0"},
    {"from":"dev10.in0","to":"dev12.out1"},
    {"from":"dev11.in0","to":"dev13.out0"},
    {"from":"dev11.in1","to":"dev2.out0"},
    {"from":"dev12.in0","to":"dev13.out1"},
    {"from":"dev12.in1","to":"dev2.out0"},
    {"from":"dev13.in0","to":"dev0.out0"},
    {"from":"dev13.in1","to":"dev3.out0"}
  ]
 }
);

// 1to8DeMux Toiminta:
// Sel2 Sel1 Sel0    A   B   C   D   E   F   G   H
//   0    0    0	In	 0	 0	 0	 0	 0	 0	 0
//   0    0    1	 0	In	 0	 0	 0	 0	 0	 0
//   0    1    0	 0	 0	In	 0	 0	 0	 0	 0
//   0    1    1	 0	 0	 0	In	 0	 0	 0	 0
//   1    0    0	 0	 0	 0	 0	In	 0	 0	 0
//   1    0    1	 0	 0	 0	 0	 0	In	 0	 0
//   1    1    0	 0	 0	 0	 0	 0	 0	In	 0
//   1    1    1	 0	 0	 0	 0	 0	 0	 0	In
simcir.registerDevice('1to8DeMux',
{
  "width":380,
  "height":200,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"1to4DeMux","id":"dev0","x":168,"y":8,"label":"1to4DeMux"},
    {"type":"1to4DeMux","id":"dev1","x":168,"y":96,"label":"1to4DeMux"},
    {"type":"In","id":"dev2","x":16,"y":8,"label":"In"},
    {"type":"1to2DeMux","id":"dev3","x":72,"y":16,"label":"1to2DeMux"},
    {"type":"Out","id":"dev4","x":248,"y":0,"label":"A"},
    {"type":"In","id":"dev5","x":16,"y":152,"label":"Sel2"},
    {"type":"In","id":"dev6","x":16,"y":56,"label":"Sel0"},
    {"type":"In","id":"dev7","x":16,"y":104,"label":"Sel1"},
    {"type":"Out","id":"dev8","x":248,"y":48,"label":"B"},
    {"type":"Out","id":"dev9","x":288,"y":16,"label":"E"},
    {"type":"Out","id":"dev10","x":288,"y":64,"label":"F"},
    {"type":"Out","id":"dev11","x":248,"y":96,"label":"C"},
    {"type":"Out","id":"dev12","x":288,"y":112,"label":"G"},
    {"type":"Out","id":"dev13","x":248,"y":144,"label":"D"},
    {"type":"Out","id":"dev14","x":288,"y":160,"label":"H"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev3.out0"},
    {"from":"dev0.in1","to":"dev6.out0"},
    {"from":"dev0.in2","to":"dev7.out0"},
    {"from":"dev1.in0","to":"dev3.out1"},
    {"from":"dev1.in1","to":"dev6.out0"},
    {"from":"dev1.in2","to":"dev7.out0"},
    {"from":"dev3.in0","to":"dev2.out0"},
    {"from":"dev3.in1","to":"dev5.out0"},
    {"from":"dev4.in0","to":"dev0.out0"},
    {"from":"dev8.in0","to":"dev0.out1"},
    {"from":"dev9.in0","to":"dev1.out0"},
    {"from":"dev10.in0","to":"dev1.out1"},
    {"from":"dev11.in0","to":"dev0.out2"},
    {"from":"dev12.in0","to":"dev1.out2"},
    {"from":"dev13.in0","to":"dev0.out3"},
    {"from":"dev14.in0","to":"dev1.out3"}
  ]
 }
);

simcir.registerDevice('Bit',
{
  "width":320,
  "height":180,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"DC","id":"dev0","x":16,"y":56,"label":"DC"},
    {"type":"PushOn","id":"dev1","x":80,"y":8,"label":"Write"},
    {"type":"Toggle","id":"dev2","x":80,"y":56,"label":"Data in","state":{"on":true}},
    {"type":"In","id":"dev3","x":128,"y":8,"label":"Write"},
    {"type":"In","id":"dev4","x":128,"y":56,"label":"Data"},
    {"type":"2to1Mux","id":"dev5","x":176,"y":32,"label":"2to1Mux"},
    {"type":"Out","id":"dev6","x":256,"y":96,"label":"Out"},
    {"type":"JK-FF","id":"dev7","x":176,"y":96,"label":"JK-FF"},
    {"type":"In","id":"dev8","x":128,"y":104,"label":"Clock"},
    {"type":"OSC","freq":1,"label":"OSC(1Hz)","id":"dev9","x":80,"y":104},
    {"type":"NOT","id":"dev10","x":256,"y":40,"label":"NOT"}
  ],
  "connectors":[
    {"from":"dev1.in0","to":"dev0.out0"},
    {"from":"dev2.in0","to":"dev0.out0"},
    {"from":"dev3.in0","to":"dev1.out0"},
    {"from":"dev4.in0","to":"dev2.out0"},
    {"from":"dev5.in0","to":"dev7.out0"},
    {"from":"dev5.in1","to":"dev3.out0"},
    {"from":"dev5.in2","to":"dev4.out0"},
    {"from":"dev6.in0","to":"dev7.out0"},
    {"from":"dev7.in0","to":"dev5.out0"},
    {"from":"dev7.in1","to":"dev8.out0"},
    {"from":"dev7.in2","to":"dev10.out0"},
    {"from":"dev8.in0","to":"dev9.out0"},
    {"from":"dev10.in0","to":"dev5.out0"}
  ]
}
);

simcir.registerDevice('4BitReg',
{
  "width":300,
  "height":300,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"Bit","id":"dev0","x":112,"y":8,"label":"Bit"},
    {"type":"Bit","id":"dev1","x":112,"y":72,"label":"Bit"},
    {"type":"Bit","id":"dev2","x":112,"y":136,"label":"Bit"},
    {"type":"Bit","id":"dev3","x":112,"y":200,"label":"Bit"},
    {"type":"BusIn","id":"dev4","x":24,"y":80,"label":"BusIn","numOutputs":4},
    {"type":"BusOut","id":"dev5","x":200,"y":120,"label":"BusOut","numInputs":4},
    {"type":"In","id":"dev6","x":32,"y":144,"label":"in"},
    {"type":"Out","id":"dev7","x":240,"y":88,"label":"out"},
    {"type":"In","id":"dev8","x":24,"y":8,"label":"load"},
    {"type":"In","id":"dev9","x":32,"y":224,"label":"clock"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev8.out0"},
    {"from":"dev0.in1","to":"dev4.out0"},
    {"from":"dev0.in2","to":"dev9.out0"},
    {"from":"dev1.in0","to":"dev8.out0"},
    {"from":"dev1.in1","to":"dev4.out1"},
    {"from":"dev1.in2","to":"dev9.out0"},
    {"from":"dev2.in0","to":"dev8.out0"},
    {"from":"dev2.in1","to":"dev4.out2"},
    {"from":"dev2.in2","to":"dev9.out0"},
    {"from":"dev3.in0","to":"dev8.out0"},
    {"from":"dev3.in1","to":"dev4.out3"},
    {"from":"dev3.in2","to":"dev9.out0"},
    {"from":"dev4.in0","to":"dev6.out0"},
    {"from":"dev5.in0","to":"dev0.out0"},
    {"from":"dev5.in1","to":"dev1.out0"},
    {"from":"dev5.in2","to":"dev2.out0"},
    {"from":"dev5.in3","to":"dev3.out0"},
    {"from":"dev7.in0","to":"dev5.out0"}
  ]
}
);

simcir.registerDevice('8BitReg',
{
  "width":420,
  "height":260,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"4BitReg","id":"dev0","x":152,"y":64,"label":"4BitReg"},
    {"type":"4BitReg","id":"dev1","x":152,"y":128,"label":"4BitReg"},
    {"type":"BusOut","id":"dev2","x":312,"y":80,"label":"BusOut"},
    {"type":"BusIn","id":"dev3","x":256,"y":72,"label":"BusIn","numOutputs":4},
    {"type":"BusIn","id":"dev4","x":256,"y":128,"label":"BusIn","numOutputs":4},
    {"type":"BusIn","id":"dev5","x":24,"y":72,"label":"BusIn"},
    {"type":"BusOut","id":"dev6","x":80,"y":128,"label":"BusOut","numInputs":4},
    {"type":"BusOut","id":"dev7","x":80,"y":72,"label":"BusOut","numInputs":4},
    {"type":"In","id":"dev8","x":96,"y":192,"label":"clock"},
    {"type":"Out","id":"dev9","x":376,"y":104,"label":"out"},
    {"type":"In","id":"dev10","x":16,"y":8,"label":"load"},
    {"type":"In","id":"dev11","x":32,"y":168,"label":"in"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev10.out0"},
    {"from":"dev0.in1","to":"dev7.out0"},
    {"from":"dev0.in2","to":"dev8.out0"},
    {"from":"dev1.in0","to":"dev10.out0"},
    {"from":"dev1.in1","to":"dev6.out0"},
    {"from":"dev1.in2","to":"dev8.out0"},
    {"from":"dev2.in0","to":"dev3.out0"},
    {"from":"dev2.in1","to":"dev3.out1"},
    {"from":"dev2.in2","to":"dev3.out2"},
    {"from":"dev2.in3","to":"dev3.out3"},
    {"from":"dev2.in4","to":"dev4.out0"},
    {"from":"dev2.in5","to":"dev4.out1"},
    {"from":"dev2.in6","to":"dev4.out2"},
    {"from":"dev2.in7","to":"dev4.out3"},
    {"from":"dev3.in0","to":"dev0.out0"},
    {"from":"dev4.in0","to":"dev1.out0"},
    {"from":"dev5.in0","to":"dev11.out0"},
    {"from":"dev6.in0","to":"dev5.out4"},
    {"from":"dev6.in1","to":"dev5.out5"},
    {"from":"dev6.in2","to":"dev5.out6"},
    {"from":"dev6.in3","to":"dev5.out7"},
    {"from":"dev7.in0","to":"dev5.out0"},
    {"from":"dev7.in1","to":"dev5.out1"},
    {"from":"dev7.in2","to":"dev5.out2"},
    {"from":"dev7.in3","to":"dev5.out3"},
    {"from":"dev9.in0","to":"dev2.out0"}
  ]
}
);

simcir.registerDevice('4BitRam8',
{
  "width":750,
  "height":450,
  "showToolbox":false,
  "toolbox":[
  ],
  "layout":{"rows":8,"cols":10,"hideLabelOnWorkspace":true,
  "nodes":{"Load":"T5","In[4]":"L3","clock":"L5","Addr[3]":"B5","Out[4]":"R5"}},
  "devices":[
    {"type":"1to8DeMux","id":"dev0","x":144,"y":144,"label":"1to8DeMux"},
    {"type":"4BitReg","id":"dev1","x":312,"y":88,"label":"4BitReg"},
    {"type":"4BitReg","id":"dev2","x":312,"y":168,"label":"4BitReg"},
    {"type":"4BitReg","id":"dev3","x":408,"y":216,"label":"4BitReg"},
    {"type":"4BitReg","id":"dev4","x":312,"y":248,"label":"4BitReg"},
    {"type":"4BitReg","id":"dev5","x":408,"y":296,"label":"4BitReg"},
    {"type":"Joint","id":"dev6","x":552,"y":184,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev7","x":544,"y":168,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev8","x":536,"y":152,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev9","x":232,"y":24,"label":"Joint","state":{"direction":0}},
    {"type":"4BitReg","id":"dev10","x":312,"y":8,"label":"4BitReg"},
    {"type":"4BitReg","id":"dev11","x":408,"y":56,"label":"4BitReg"},
    {"type":"Joint","id":"dev12","x":240,"y":96,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev13","x":240,"y":64,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev14","x":240,"y":144,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev15","x":240,"y":176,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev16","x":264,"y":136,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev17","x":536,"y":344,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev18","x":528,"y":352,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev19","x":520,"y":360,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev20","x":216,"y":360,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev21","x":232,"y":344,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev22","x":216,"y":280,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev23","x":224,"y":352,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev24","x":240,"y":224,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev25","x":240,"y":256,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev26","x":240,"y":304,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev27","x":272,"y":56,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev28","x":272,"y":88,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev29","x":272,"y":296,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev30","x":272,"y":248,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev31","x":272,"y":216,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev32","x":384,"y":336,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev33","x":376,"y":384,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev34","x":384,"y":256,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev35","x":384,"y":176,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev36","x":384,"y":96,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev37","x":288,"y":48,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev38","x":288,"y":128,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev39","x":288,"y":288,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev40","x":280,"y":384,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev41","x":288,"y":208,"label":"Joint","state":{"direction":3}},
    {"type":"Joint","id":"dev42","x":272,"y":8,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev43","x":208,"y":288,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev44","x":200,"y":296,"label":"Joint","state":{"direction":0}},
    {"type":"8to1Mux4","id":"dev45","x":576,"y":24,"label":"8to1Mux4"},
    {"type":"Joint","id":"dev46","x":560,"y":208,"label":"Joint","state":{"direction":2}},
    {"type":"Joint","id":"dev47","x":488,"y":40,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev48","x":488,"y":56,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev49","x":472,"y":104,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev50","x":480,"y":152,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev51","x":496,"y":72,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev52","x":504,"y":88,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev53","x":488,"y":184,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev54","x":512,"y":104,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev55","x":496,"y":232,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev56","x":520,"y":120,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev57","x":504,"y":264,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev58","x":528,"y":136,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev59","x":512,"y":312,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev60","x":648,"y":104,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev61","x":656,"y":200,"label":"Joint","state":{"direction":1}},
    {"type":"Joint","id":"dev62","x":256,"y":168,"label":"Joint","state":{"direction":0}},
    {"type":"4BitReg","id":"dev63","x":408,"y":136,"label":"4BitReg"},
    {"type":"In","id":"dev64","x":224,"y":376,"label":"clock"},
    {"type":"Out","id":"dev65","x":560,"y":232,"label":"Out[4]"},
    {"type":"In","id":"dev66","x":168,"y":16,"label":"In[4]"},
    {"type":"Joint","id":"dev67","x":112,"y":192,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev68","x":120,"y":288,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev69","x":104,"y":208,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev70","x":128,"y":296,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev71","x":112,"y":280,"label":"Joint","state":{"direction":0}},
    {"type":"Joint","id":"dev72","x":96,"y":224,"label":"Joint","state":{"direction":0}},
    {"type":"BusIn","id":"dev73","x":64,"y":192,"label":"BusIn","numOutputs":3},
    {"type":"In","id":"dev74","x":96,"y":136,"label":"Load"},
    {"type":"In","id":"dev75","x":8,"y":160,"label":"Addr[3]"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev74.out0"},
    {"from":"dev0.in1","to":"dev67.out0"},
    {"from":"dev0.in2","to":"dev69.out0"},
    {"from":"dev0.in3","to":"dev72.out0"},
    {"from":"dev1.in0","to":"dev28.out0"},
    {"from":"dev1.in1","to":"dev12.out0"},
    {"from":"dev1.in2","to":"dev38.out0"},
    {"from":"dev2.in0","to":"dev62.out0"},
    {"from":"dev2.in1","to":"dev15.out0"},
    {"from":"dev2.in2","to":"dev41.out0"},
    {"from":"dev3.in0","to":"dev31.out0"},
    {"from":"dev3.in1","to":"dev24.out0"},
    {"from":"dev3.in2","to":"dev34.out0"},
    {"from":"dev4.in0","to":"dev30.out0"},
    {"from":"dev4.in1","to":"dev25.out0"},
    {"from":"dev4.in2","to":"dev39.out0"},
    {"from":"dev5.in0","to":"dev29.out0"},
    {"from":"dev5.in1","to":"dev26.out0"},
    {"from":"dev5.in2","to":"dev32.out0"},
    {"from":"dev6.in0","to":"dev17.out0"},
    {"from":"dev7.in0","to":"dev18.out0"},
    {"from":"dev8.in0","to":"dev19.out0"},
    {"from":"dev9.in0","to":"dev66.out0"},
    {"from":"dev10.in0","to":"dev42.out0"},
    {"from":"dev10.in1","to":"dev9.out0"},
    {"from":"dev10.in2","to":"dev37.out0"},
    {"from":"dev11.in0","to":"dev27.out0"},
    {"from":"dev11.in1","to":"dev13.out0"},
    {"from":"dev11.in2","to":"dev36.out0"},
    {"from":"dev12.in0","to":"dev13.out0"},
    {"from":"dev13.in0","to":"dev9.out0"},
    {"from":"dev14.in0","to":"dev12.out0"},
    {"from":"dev15.in0","to":"dev14.out0"},
    {"from":"dev16.in0","to":"dev0.out3"},
    {"from":"dev17.in0","to":"dev21.out0"},
    {"from":"dev18.in0","to":"dev23.out0"},
    {"from":"dev19.in0","to":"dev20.out0"},
    {"from":"dev20.in0","to":"dev44.out0"},
    {"from":"dev21.in0","to":"dev22.out0"},
    {"from":"dev22.in0","to":"dev71.out0"},
    {"from":"dev23.in0","to":"dev43.out0"},
    {"from":"dev24.in0","to":"dev15.out0"},
    {"from":"dev25.in0","to":"dev24.out0"},
    {"from":"dev26.in0","to":"dev25.out0"},
    {"from":"dev27.in0","to":"dev0.out1"},
    {"from":"dev28.in0","to":"dev0.out2"},
    {"from":"dev29.in0","to":"dev0.out7"},
    {"from":"dev30.in0","to":"dev0.out6"},
    {"from":"dev31.in0","to":"dev0.out5"},
    {"from":"dev32.in0","to":"dev33.out0"},
    {"from":"dev33.in0","to":"dev40.out0"},
    {"from":"dev34.in0","to":"dev32.out0"},
    {"from":"dev35.in0","to":"dev34.out0"},
    {"from":"dev36.in0","to":"dev35.out0"},
    {"from":"dev37.in0","to":"dev38.out0"},
    {"from":"dev38.in0","to":"dev41.out0"},
    {"from":"dev39.in0","to":"dev40.out0"},
    {"from":"dev40.in0","to":"dev64.out0"},
    {"from":"dev41.in0","to":"dev39.out0"},
    {"from":"dev42.in0","to":"dev0.out0"},
    {"from":"dev43.in0","to":"dev68.out0"},
    {"from":"dev44.in0","to":"dev70.out0"},
    {"from":"dev45.in0","to":"dev10.out0"},
    {"from":"dev45.in1","to":"dev47.out0"},
    {"from":"dev45.in2","to":"dev48.out0"},
    {"from":"dev45.in3","to":"dev51.out0"},
    {"from":"dev45.in4","to":"dev52.out0"},
    {"from":"dev45.in5","to":"dev54.out0"},
    {"from":"dev45.in6","to":"dev56.out0"},
    {"from":"dev45.in7","to":"dev58.out0"},
    {"from":"dev45.in8","to":"dev8.out0"},
    {"from":"dev45.in9","to":"dev7.out0"},
    {"from":"dev45.in10","to":"dev6.out0"},
    {"from":"dev46.in0","to":"dev61.out0"},
    {"from":"dev47.in0","to":"dev11.out0"},
    {"from":"dev48.in0","to":"dev49.out0"},
    {"from":"dev49.in0","to":"dev1.out0"},
    {"from":"dev50.in0","to":"dev63.out0"},
    {"from":"dev51.in0","to":"dev50.out0"},
    {"from":"dev52.in0","to":"dev53.out0"},
    {"from":"dev53.in0","to":"dev2.out0"},
    {"from":"dev54.in0","to":"dev55.out0"},
    {"from":"dev55.in0","to":"dev3.out0"},
    {"from":"dev56.in0","to":"dev57.out0"},
    {"from":"dev57.in0","to":"dev4.out0"},
    {"from":"dev58.in0","to":"dev59.out0"},
    {"from":"dev59.in0","to":"dev5.out0"},
    {"from":"dev60.in0","to":"dev45.out0"},
    {"from":"dev61.in0","to":"dev60.out0"},
    {"from":"dev62.in0","to":"dev0.out4"},
    {"from":"dev63.in0","to":"dev16.out0"},
    {"from":"dev63.in1","to":"dev14.out0"},
    {"from":"dev63.in2","to":"dev35.out0"},
    {"from":"dev65.in0","to":"dev46.out0"},
    {"from":"dev67.in0","to":"dev73.out0"},
    {"from":"dev68.in0","to":"dev69.out0"},
    {"from":"dev69.in0","to":"dev73.out1"},
    {"from":"dev70.in0","to":"dev67.out0"},
    {"from":"dev71.in0","to":"dev72.out0"},
    {"from":"dev72.in0","to":"dev73.out2"},
    {"from":"dev73.in0","to":"dev75.out0"}
  ]
}
);


simcir.registerDevice('Multiplier',
{
  "width":420,
  "height":260,
  "showToolbox":false,
  "toolbox":[
  ],
  "devices":[
    {"type":"In","id":"dev0","x":96,"y":16,"label":"x"},
    {"type":"In","id":"dev1","x":96,"y":160,"label":"y"},
    {"type":"In","id":"dev2","x":96,"y":64,"label":"Cin"},
    {"type":"In","id":"dev3","x":96,"y":112,"label":"Sin"},
    {"type":"Toggle","id":"dev4","x":48,"y":16,"label":"Toggle","state":{"on":false}},
    {"type":"Toggle","id":"dev5","x":48,"y":160,"label":"Toggle","state":{"on":false}},
    {"type":"Toggle","id":"dev6","x":48,"y":112,"label":"Toggle","state":{"on":false}},
    {"type":"Toggle","id":"dev7","x":48,"y":64,"label":"Toggle","state":{"on":false}},
    {"type":"FullAdder","id":"dev8","x":232,"y":32,"label":"FullAdder"},
    {"type":"DC","id":"dev9","x":8,"y":16,"label":"DC"},
    {"type":"AND","id":"dev10","x":152,"y":56,"label":"AND"},
    {"type":"Out","id":"dev11","x":312,"y":16,"label":"Sout"},
    {"type":"Out","id":"dev12","x":312,"y":64,"label":"x"},
    {"type":"Out","id":"dev13","x":312,"y":112,"label":"Cout"},
    {"type":"Out","id":"dev14","x":312,"y":160,"label":"y"}
  ],
  "connectors":[
    {"from":"dev0.in0","to":"dev4.out0"},
    {"from":"dev1.in0","to":"dev5.out0"},
    {"from":"dev2.in0","to":"dev7.out0"},
    {"from":"dev3.in0","to":"dev6.out0"},
    {"from":"dev4.in0","to":"dev9.out0"},
    {"from":"dev5.in0","to":"dev9.out0"},
    {"from":"dev6.in0","to":"dev9.out0"},
    {"from":"dev7.in0","to":"dev9.out0"},
    {"from":"dev8.in0","to":"dev2.out0"},
    {"from":"dev8.in1","to":"dev10.out0"},
    {"from":"dev8.in2","to":"dev3.out0"},
    {"from":"dev10.in0","to":"dev0.out0"},
    {"from":"dev10.in1","to":"dev1.out0"},
    {"from":"dev11.in0","to":"dev8.out0"},
    {"from":"dev12.in0","to":"dev0.out0"},
    {"from":"dev13.in0","to":"dev8.out1"},
    {"from":"dev14.in0","to":"dev1.out0"}
  ]
});