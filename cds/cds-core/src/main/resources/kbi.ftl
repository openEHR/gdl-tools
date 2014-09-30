<!DOCTYPE html>
<html lang=${language}>
<head><meta charset='UTF-8'>
    <title>${kbInstance.kbiId}</title>
    <style>
        body {font-family: Calibri;}
        h1 {font-size: 26px; margin:0px;}
        h2 {background-color: #92b842; color: white; padding: 5px;  font-size: 110%; margin-bottom:0px; margin-top:10px; font-weight: bold;}
        .details-label {font-weight: bold; margin-right: 5px;}
    </style>
</head>
<body>
<h1>
    ${kbInstance_definitions.name?if_exists}
</h1>
<h2>${texts.KBInstanceDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Description}:</td><td align='left'>${kbInstance_definitions.description?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Purpose}:</td><td align='left'>${kbInstance_description.purpose?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Use}:</td><td align='left'>${kbInstance_description.use?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Misuse}:</td><td align='left'>${kbInstance_description.misuse?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.References}:</td><td align='left'>${kbInstance.resourceDescription.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<h2>${texts.AuthorDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Name}:</td><td align='left'>${kbInstance.resourceDescription.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Email}:</td><td align='left'>${kbInstance.resourceDescription.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Organisation}:</td><td align='left'>${kbInstance.resourceDescription.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Date}:</td><td align='left'>${kbInstance.resourceDescription.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.AuthorshipLifecycle}:</td><td align='left'>${kbInstance.resourceDescription.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Copyright}:</td><td align='left'>${kbInstance_description.copyright?if_exists}</td></tr>
</table>
<#if kbInstance_description.keywords?has_content>
    <h2>${texts.Keywords?upper_case}</h2>
    <#list kbInstance_description.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if kbInstance_description.otherContributors?has_content>
     <h2>${texts.Contributors?upper_case}</h2>
    <#list kbInstance_description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
</#if>
<h2>${texts.KBInstanceDefinition?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Guidelines}:</td><td align='left'><#list kbInstance.guideIds as guideId><i>${guideId}<#if guideId_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td class="details-label">${texts.Filters}:</td><td align='left'><#list kbInstance.filters as filter><i>${getGTCodeText(filter)}<#if filter_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td class="details-label">${texts.Indicators}:</td><td align='left'><#list kbInstance.indicators as indicator><i>${getGTCodeText(indicator)}<#if indicator_has_next>, </#if></i></#list></td></tr>
</table>
</body>
</html>
