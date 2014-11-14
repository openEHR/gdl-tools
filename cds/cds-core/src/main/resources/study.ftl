<!DOCTYPE html>
<html lang=${language}>
<head><meta charset='UTF-8'>
    <title>${study.studyId}</title>
    <style>
        body {font-family: Calibri;}
        h1 {font-size: 26px; margin:0px;}
        h2 {background-color: #92b842; color: white; padding: 5px;  font-size: 110%; margin-bottom:0px; margin-top:10px; font-weight: bold;}
        .details-label {font-weight: bold; margin-right: 5px;}
    </style>
</head>
<body>
<h1>
    ${study_definitions.name?if_exists}
</h1>
<h2>${texts.StudyDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Description}:</td><td align='left'>${study_definitions.description?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Purpose}:</td><td align='left'>${study_description.purpose?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Use}:</td><td align='left'>${study_description.use?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Misuse}:</td><td align='left'>${study_description.misuse?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.References}:</td><td align='left'>${study.resourceDescription.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<h2>${texts.AuthorDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Name}:</td><td align='left'>${study.resourceDescription.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Email}:</td><td align='left'>${study.resourceDescription.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Organisation}:</td><td align='left'>${study.resourceDescription.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Date}:</td><td align='left'>${study.resourceDescription.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.AuthorshipLifecycle}:</td><td align='left'>${study.resourceDescription.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Copyright}:</td><td align='left'>${study_description.copyright?if_exists}</td></tr>
</table>
<#if study_description.keywords?has_content>
    <h2>${texts.Keywords?upper_case}</h2>
    <#list study_description.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if study_description.otherContributors?has_content>
     <h2>${texts.Contributors?upper_case}</h2>
    <#list study_description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
</#if>
<h2>${texts.StudyCriteria?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.InclusionCriteria}:</td><td align='left'>${study_definitions.inclusionCriteria?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.ExclusionCriteria}:</td><td align='left'>${study_definitions.exclusionCriteria?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<h2>${texts.StudyDefinition?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Guidelines}:</td><td align='left'><#list study.guideIds as guideId><i>${guideId}<#if guideId_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td class="details-label">${texts.Filters}:</td><td align='left'><#list study.filters as filter><i>${getGTCodeText(filter)}<#if filter_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td class="details-label">${texts.Indicators}:</td><td align='left'><#list study.indicators as indicator><i>${getGTCodeText(indicator)}<#if indicator_has_next>, </#if></i></#list></td></tr>
</table>
</body>
</html>
