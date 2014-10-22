<!DOCTYPE html>
<html lang=${language}>
<head><meta charset='UTF-8'>
    <title>${dsv.dsViewId}</title>
    <style>
        body {font-family: Calibri;}
        h1 {font-size: 26px; margin:0px;}
        h2 {background-color: #af6ebd; color: white; padding: 5px;  font-size: 110%; margin-bottom:0px; margin-top:10px; font-weight: bold;}
        h3 {background-color: #b799bd;  padding-bottom: 8px; padding-left: 5px; margin-bottom:0px; margin-top:0px;}
        .details-label {font-weight: bold; margin-right: 5px;}
    </style>
</head>
<body>
<h1>
    ${dsv_definitions.name?if_exists}
</h1>
<h2>${texts.DSViewDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Description}:</td><td align='left'>${dsv_definitions.description?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Purpose}:</td><td align='left'>${dsv_description.purpose?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Use}:</td><td align='left'>${dsv_description.use?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Misuse}:</td><td align='left'>${dsv_description.misuse?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.References}:</td><td align='left'>${dsv.resourceDescription.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<h2>${texts.AuthorDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Name}:</td><td align='left'>${dsv.resourceDescription.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Email}:</td><td align='left'>${dsv.resourceDescription.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Organisation}:</td><td align='left'>${dsv.resourceDescription.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Date}:</td><td align='left'>${dsv.resourceDescription.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.AuthorshipLifecycle}:</td><td align='left'>${dsv.resourceDescription.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Copyright}:</td><td align='left'>${dsv_description.copyright?if_exists}</td></tr>
</table>
<#if dsv_description.keywords?has_content>
    <h2>${texts.Keywords?upper_case}</h2>
    <#list dsv_description.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if dsv_description.otherContributors?has_content>
     <h2>${texts.Contributors?upper_case}</h2>
    <#list dsv_description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
</#if>
<h2>${texts.DSVDefinition?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.AlertGuidelines}:</td><td align='left'><#list dsv.alertGuideIds as guideId><i>${guideId}<#if guideId_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td class="details-label">${texts.ExecutionGuidelines}:</td><td align='left'><#list dsv.executionGuideIds as guideId><i>${guideId}<#if guideId_has_next>, </#if></i></#list></td></tr>
</table>
<#if dsv.alertBindings?has_content>
    <h2>${texts.Alerts?upper_case}</h2>
    <#list dsv.alertBindings?values as terminologyAlertBinding>
        <h3>${terminologyAlertBinding.terminologyId}</h3>
        <div class="bg-block">
             <#list terminologyAlertBinding.terminologyAlertBindings?values as dvCodedText>
                ${dvCodedText.value} (${dvCodedText.code})<br>
             </#list>
        </div>
        <br>
    </#list>
    <br>
</#if>
</body>
</html>
