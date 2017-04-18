<!DOCTYPE html>
<html lang=${language}>
<head><meta charset='UTF-8'>
    <title>${guide.id}</title>
    <style>
        body {font-family: Calibri;}
        td { vertical-align: top; }
        h1 {font-size: 26px; margin:0px;}
        h2 {background-color: #4f81bd; color: white; padding: 5px;  font-size: 110%; margin-bottom:0px; margin-top:10px; font-weight: bold;}
        h3 {background-color: #9ec2ef;  padding-bottom: 8px; padding-left: 5px; margin-bottom:0px; margin-top:0px;}
        .bg-block {background-color: #dbe5f1;  padding-bottom: 8px; margin-bottom: 5px;;}
        .details-label {font-weight: bold; margin-right: 5px;}
    </style>
</head>
<body>
<h1>
    ${guide_terms["guide.concept"?eval].text?if_exists}
</h1>
<h2>${texts.GuideDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Description}:</td><td align='left'>${guide_terms["guide.concept"?eval].description?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Purpose}:</td><td align='left'>${guide_details.purpose?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Use}:</td><td align='left'>${guide_details.use?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Misuse}:</td><td align='left'>${guide_details.misuse?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td class="details-label">${texts.References}:</td><td align='left'>${guide.description.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<h2>${texts.AuthorDetails?upper_case}</h2>
<table>
    <tr valign='top'><td class="details-label">${texts.Name}: </td><td align='left'>${guide.description.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Email}: </td><td align='left'>${guide.description.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Organisation}: </td><td align='left'>${guide.description.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Date}: </td><td align='left'>${guide.description.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.AuthorshipLifecycle}: </td><td align='left'>${guide.description.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td class="details-label">${texts.Copyright}: </td><td align='left'>${guide_details.copyright?if_exists}</td></tr>
</table>
<#if guide_details.keywords?has_content>
    <h2>${texts.Keywords?upper_case}</h2>
    <#list guide_details.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if guide.description.otherContributors?has_content>
    <h2>${texts.Contributors?upper_case}</h2>
    <#list guide.description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
    <br>
</#if>

<h2>${texts.Definitions?upper_case}</h2>
<#if guide_definitions_ehr?has_content>
    <h3>EHR</h3>
    <#list guide_definitions_ehr as definitionRuleLine>
    <div class="bg-block">
    ${definitionRuleLine}
    </div>
    </#list>
</#if>
<#if guide_definitions_cds?has_content>
<h3>CDS</h3>
    <#list guide_definitions_cds as definitionRuleLine>
    <div class="bg-block">
    ${definitionRuleLine}
    </div>
    </#list>
</#if>
<#if guide_definitions_any?has_content>
<h3>ANY</h3>
    <#list guide_definitions_any as definitionRuleLine>
    <div class="bg-block">
    ${definitionRuleLine}
    </div>
    </#list>
</#if>

<br>
<#if guide_preconditions?has_content>
    <h2>${texts.Preconditions?upper_case}</h2>
        <#list guide_preconditions as preconditionRuleLine>
        <div class="bg-block">
            ${preconditionRuleLine}
        </div>
    </#list>
    <br>
</#if>

<#if guide_default_actions?has_content>
    <h2>${texts.Defaults?upper_case}</h2>
        <#list guide_default_actions as defaultRuleLine>
        <div class="bg-block">
            ${defaultRuleLine}
        </div>
        </#list>
    <br>
</#if>
<#if guide_rules?? && guide_rules?has_content>
    <h2>${texts.RuleList?upper_case}</h2>
    <#list guide_rules as readableGuide>
        <div class="bg-block">
            ${readableGuide}
        </div>
    </#list>
</#if>
<#if guide.ontology.termBindings?? && guide.ontology.termBindings?has_content>
    <h2>${texts.Bindings?upper_case}</h2>
    <#list guide.ontology.termBindings?values as termBinding>
        <h3>${termBinding.id}</h3>
        <div class="bg-block">
            <table>
                <#list termBinding.bindings?values as binding>
                 <tr><td class="details-label">${guide_terms["binding.id"?eval].text}:</td>
                 <td>
                 <#list binding.codes as code>
                    ${code.codeString}<#if code_has_next>, </#if>
                 </#list>
                 </td></tr>
                </#list>
            </table>
        </div>
        <br>
    </#list>
    <br>
</#if>
</body>
</html>
