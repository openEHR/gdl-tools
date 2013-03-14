package se.cambio.cds.gdl.parser;

import java.io.*;
import java.util.*;

import org.openehr.am.parser.ContentObject;
import org.openehr.am.parser.DADLParser;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.*;

public class GDLParser {

	public Guide parse(InputStream input) throws Exception {
		DADLParser parser = new DADLParser(input);
		ContentObject content = parser.parse();
		GDLBinding binding = new GDLBinding();
		
		Object obj = binding.bind(content);
		Guide guide = (Guide) obj;
		bindExpressions(guide);
		return guide;
	}

	public Guide parse(Reader input) throws Exception {
		DADLParser parser = new DADLParser(input);
		ContentObject content = parser.parse();
		GDLBinding binding = new GDLBinding();
		Object obj = binding.bind(content);
		Guide guide = (Guide) obj;
		bindExpressions(guide);
		return guide;
	}

	/*
	 * List of expressions in GDL 1. guide definition pre-conditions 2.
	 * archetype binding predicates 3. rule when statements 4. rule then
	 * statements
	 */
	private void bindExpressions(Guide guide) throws Exception {
		List<String> preConditions = guide.getDefinition().getPreConditions();

		guide.getDefinition().setPreConditionExpressions(
				parseExpressions(preConditions));

		GuideDefinition definition = guide.getDefinition();
		if (definition.getArchetypeBindings() != null) {
			Collection<ArchetypeBinding> bindings = definition
					.getArchetypeBindings();
			for (ArchetypeBinding binding : bindings) {
				binding.setPredicateStatements(parseExpressions(binding
						.getPredicates()));
			}
			if (definition.getRules() != null) {
				Collection<Rule> rules = definition.getRules().values();
				for (Rule rule : rules) {
					rule.setWhenStatements(parseExpressions(rule.getWhen()));
					rule.setThenStatements(toAssignments(parseExpressions(rule
							.getThen())));
				}
			}
		}
	}

	private List<AssignmentExpression> toAssignments(List<ExpressionItem> items) {
		List<AssignmentExpression> ret = new ArrayList<AssignmentExpression>();
		if (items != null) {
			for (ExpressionItem item : items) {
				ret.add((AssignmentExpression) item);
			}
		}
		return ret;
	}

	private List<ExpressionItem> parseExpressions(List<String> lines)
			throws Exception {
		if (lines == null) {
			return null;
		}
		List<ExpressionItem> items = new ArrayList<ExpressionItem>();
		for (String line : lines) {
			items.add(Expressions.parse(line));
		}
		return items;
	}
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */