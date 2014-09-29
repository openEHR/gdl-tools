<!DOCTYPE html>
<html lang=${language}>
<head><meta charset='UTF-8'>
    <title>${terminologyId}</title>
    <style>
        body {font-family: Calibri;}
        h1 {font-size: 26px; margin:0px;}
        h2 {background-color: #1ba77a; color: white; padding: 5px;  font-size: 110%; margin-bottom:0px; margin-top:10px; font-weight: bold;}
        .tree,
        .tree ul {
          margin:0;
          padding:0;
          list-style:none;
        }
        .tree ul {
          margin-left:1em; /* indentation */
          position:relative;
        }
        .tree ul ul {margin-left:.5em} /* (indentation/2) */
        .tree ul:before {
          content:"";
          display:block;
          width:0;
          position:absolute;
          top:0;
          bottom:0;
          left:0;
          border-left:1px solid;
        }
        .tree li {
          margin:0;
          padding:0 .8em;
          line-height:2em; /* default list item's `line-height` */
          color:#369;
          font-weight:bold;
          position:relative;
        }
        .tree ul li:before {
          content:"";
          display:block;
          width:10px; /* same with indentation */
          height:0;
          border-top:1px solid;
          margin-top:-1px; /* border top width */
          position:absolute;
          top:1em; /* (line-height/2) */
          left:0;
        }
        .tree ul li:last-child:before {
          background:white; /* same with body background */
          height:auto;
          top:1em; /* (line-height/2) */
          bottom:0;
        }
    </style>
</head>
<body>
<h1>
    ${terminologyId}
</h1>
 <h2>${texts.TerminologyDefinition?upper_case}</h2>
 ${terminology_definition}
</body>
</html>
