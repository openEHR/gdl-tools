<!DOCTYPE html>
<HTML lang=${language}>
<HEAD><meta charset='UTF-8'>
    <TITLE>${study.studyId}</TITLE>
    <STYLE>
        body {font-family: Calibri;}
        .title1 {font-weight: bold; font-size: 26px; padding-bottom: 4px;}
        .header1 {background-color: #92b842; color: white; padding: 5px;  font-size: 110%;}
    </STYLE>
</HEAD>
<BODY>
<div class="title1">
    ${study_definitions.name?if_exists}
</div>
<div class="header1"><b>${texts.StudyDetails?upper_case}</b></div>
<table>
    <tr valign='top'><td><b>${texts.Description}: </b></td><td align='left'>${study_definitions.description?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Purpose}: </b></td><td align='left'>${study_description.purpose?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.Use}: </b></td><td align='left'>${study_description.use?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.Misuse}: </b></td><td align='left'>${study_description.misuse?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.References}: </b></td><td align='left'>${study.resourceDescription.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<br><div class="header1"><b>${texts.AuthorDetails?upper_case}</b></div>
<table>
    <tr valign='top'><td><b>${texts.Name}: </b></td><td align='left'>${study.resourceDescription.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Email}: </b></td><td align='left'>${study.resourceDescription.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Organisation}: </b></td><td align='left'>${study.resourceDescription.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Date}: </b></td><td align='left'>${study.resourceDescription.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.AuthorshipLifecycle}: </b></td><td align='left'>${study.resourceDescription.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Copyright}: </b></td><td align='left'>${study_description.copyright?if_exists}</td></tr>
</table>
<#if study_description.keywords?has_content>
    <br>
    <div class="header1"><b>${texts.Keywords?upper_case}</b></div>
    <#list study_description.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if study_description.otherContributors?has_content>
     <br>
     <div class="header1"><b>${texts.Contributors?upper_case}</b></div>
    <#list study_description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
</#if>
<br>
<div class="header1"><b>${texts.StudyCriteria?upper_case}</b></div>
<table>
    <tr valign='top'><td><b>${texts.InclusionCriteria}: </b></td><td align='left'>${study_definitions.inclusionCriteria?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.ExclusionCriteria}: </b></td><td align='left'>${study_definitions.exclusionCriteria?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<br>
<div class="header1"><b>${texts.StudyDefinition?upper_case}</b></div>
<table>
    <tr valign='top'><td><b>${texts.Guidelines}: </b></td><td align='left'><#list study.guideIds as guideId><i>${guideId}<#if guideId_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td><b>${texts.Filters}: </b></td><td align='left'><#list study.filters as filter><i>${getGTCodeText(filter)}<#if filter_has_next>, </#if></i></#list></td></tr>
    <tr valign='top'><td><b>${texts.Indicators}: </b></td><td align='left'><#list study.indicators as indicator><i>${getGTCodeText(indicator)}<#if indicator_has_next>, </#if></i></#list></td></tr>
</table>

</BODY>
</HTML>
