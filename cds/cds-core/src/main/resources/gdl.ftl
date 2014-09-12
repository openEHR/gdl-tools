<!DOCTYPE html>
<HTML lang=${language}>
<HEAD><meta charset='UTF-8'>
    <TITLE>${guide.id}</TITLE>
    <STYLE>
        body {font-family: Calibri;}
        .title1 {font-weight: bold; font-size: 26px; padding-bottom: 4px;}
        .header1 {background-color: #4f81bd; color: white; padding: 5px;  font-size: 110%;}
        .header2 {background-color: #9ec2ef;  padding-bottom: 8px; padding-left: 5px;}
        .container1 {background-color: #dbe5f1;  padding-bottom: 8px;}
    </STYLE>
</HEAD>
<BODY>
<div class="title1">
    ${guide_terms["guide.concept"?eval].text?if_exists}
</div>
<div class="header1"><b>${texts.GuideDetails?upper_case}</b></div>
<table>
    <tr valign='top'><td><b>${texts.Description}: </b></td><td align='left'>${guide_terms["guide.concept"?eval].description?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Purpose}: </b></td><td align='left'>${guide_details.purpose?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.Use}: </b></td><td align='left'>${guide_details.use?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.Misuse}: </b></td><td align='left'>${guide_details.misuse?if_exists?replace("\n", "<br>")}</td></tr>
    <tr valign='top'><td><b>${texts.References}: </b></td><td align='left'>${guide.description.otherDetails.references?if_exists?replace("\n", "<br>")}</td></tr>
</table>
<br><div class="header1"><b>${texts.AuthorDetails?upper_case}</b></div>
<table>
    <tr valign='top'><td><b>${texts.Name}: </b></td><td align='left'>${guide.description.originalAuthor.name?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Email}: </b></td><td align='left'>${guide.description.originalAuthor.email?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Organisation}: </b></td><td align='left'>${guide.description.originalAuthor.organisation?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Date}: </b></td><td align='left'>${guide.description.originalAuthor.date?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.AuthorshipLifecycle}: </b></td><td align='left'>${guide.description.lifecycleState?if_exists}</td></tr>
    <tr valign='top'><td><b>${texts.Copyright}: </b></td><td align='left'>${guide_details.copyright?if_exists}</td></tr>
</table>
<#if guide_details.keywords?has_content>
    <br><div class="header1"><b>${texts.Keywords?upper_case}</b></div>
    <#list guide_details.keywords as keyword><i>
        ${keyword}<#if keyword_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if guide.description.otherContributors?has_content>
    <br><div class="header1"><b>${texts.Contributors?upper_case}</b></div>
    <#list guide.description.otherContributors as otherContributor><i>
        ${otherContributor}<#if otherContributor_has_next>, </#if>
    </i></#list>
    <br>
</#if>
<#if guide_definitions.preconditionRuleLines?? && guide_definitions.preconditionRuleLines?has_content>
    <br><div class="header1"><b>${texts.Preconditions?upper_case}</b></div>
        <#list guide_definitions.preconditionRuleLines as preconditionRuleLine>
        <div class="container1">
            ${preconditionRuleLine}
        </div>
    </#list>
    <br>
</#if>
<#if guide_definitions.readableRules?? && guide_definitions.readableRules?has_content>
    <br><div class="header1"><b>${texts.RuleList?upper_case}</b></div>
    <#list guide_definitions.readableRules?values as readableRule>
        <div class="container1">
            ${readableRule}
        </div><br>
    </#list>
</#if>
<#if guide.ontology.termBindings?? && guide.ontology.termBindings?has_content>
    <div class="header1"><b>${texts.Bindings?upper_case}</b></div>
    <#list guide.ontology.termBindings?values as termBinding>
        <div class="header2">
            ${termBinding.id}
        </div>
        <div class="container1">
            <#list termBinding.bindings?values as binding>
                 <b>${guide_terms["binding.id"?eval].text}: </b>
                 <#list binding.codes as code>
                    ${code.codeString}<#if code_has_next>, </#if>
                 </#list>
                 <br>
            </#list>
        </div>
        <br>
    </#list>
    <br>
</#if>
</BODY>
</HTML>
