<!DOCTYPE html>
<html lang=${language}>
<head><meta charset='UTF-8'>
    <title>${archetype.archetypeId.value}</title>
    <style>
        body {font-family: Calibri;}
        h1 {font-size: 26px; margin:0px;}
        h2 {background-color: #4f81bd; color: white; padding: 5px;  font-size: 110%; margin-bottom:0px; margin-top:10px; font-weight: bold;}
        .details-label {font-weight: bold; margin-right: 5px;}
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
    ${getATCodeText(archetype.concept)?if_exists}
</h1>
<h2>${texts.ArchetypeDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Description}:</td><td align='left'>${getATCodeText(archetype.concept)?if_exists}</td></tr>
        <tr valign='top'><td class="details-label">${texts.Purpose}:</td><td align='left'>${archetype_description_item.purpose?if_exists?replace("\n", "<br>")}</td></tr>
        <tr valign='top'><td class="details-label">${texts.Use}:</td><td align='left'>${archetype_description_item.use?if_exists?replace("\n", "<br>")}</td></tr>
        <tr valign='top'><td class="details-label">${texts.Misuse}:</td><td align='left'>${archetype_description_item.misuse?if_exists?replace("\n", "<br>")}</td></tr>
        <tr valign='top'><td class="details-label">${texts.References}:</td><td align='left'>${archetype.description.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<br><h2>${texts.AuthorDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Name}:</td><td align='left'>${archetype.description.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Email}:</td><td align='left'>${archetype.description.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Organisation}:</td><td align='left'>${archetype.description.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Date}:</td><td align='left'>${archetype.description.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.AuthorshipLifecycle}:</td><td align='left'>${archetype.description.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Copyright}:</td><td align='left'>${archetype_description_item.copyright?if_exists}</td></tr>
</table>
<#if archetype_description_item.keywords?has_content>
    <h2>${texts.Keywords?upper_case}</h2>
    <#list archetype_description_item.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if archetype.description.otherContributors?has_content>
     <h2>${texts.Contributors?upper_case}</h2>
    <#list archetype.description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
    <br>
</#if>

 <br>
 <div class="header1">${texts.ArchetypeDefinition?upper_case}</h2>
 ${archetype_definition}
</body>
</html>
