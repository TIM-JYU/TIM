// SimcirJS - oma kirjasto
// Komponentit
//  2to1Mux
//  4to1Mux
//  1to2DeMux
//  2to4DeMux

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
