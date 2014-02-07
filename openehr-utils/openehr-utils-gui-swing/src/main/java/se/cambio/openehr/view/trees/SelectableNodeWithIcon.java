/*
 * Creado el 11-dic-2007
 *


 */
package se.cambio.openehr.view.trees;

import javax.swing.Icon;
import javax.swing.ImageIcon;

/**
 * @author icorram
 *


 */
public class SelectableNodeWithIcon<E> extends SelectableNode<E> {

    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 7032008L;
    /**
     * 
     */
    private Icon _icono = null;

    public SelectableNodeWithIcon() {
	super();
    }

    public SelectableNodeWithIcon(String descripcion, Icon icono) {
	super(descripcion);
	_icono = icono;
    }

    public SelectableNodeWithIcon(String descripcion,
	    E objeto, Boolean seleccionado, Icon icono) {
	super(descripcion,objeto,seleccionado);
	_icono = icono;
    }

    public SelectableNodeWithIcon(String descripcion,
	    E objeto, Boolean seleccionUnica, Boolean seleccionado, Icon icono) {
	super(descripcion,objeto,seleccionUnica, seleccionado);
	_icono = icono;
    }

    public SelectableNodeWithIcon(String descripcion,
	    E objeto, Boolean seleccionUnica, Boolean seleccionado, Icon icono, String tooltip) {
	super(descripcion,objeto,seleccionUnica, seleccionado, tooltip);
	_icono = icono;
    }

    public SelectableNodeWithIcon(String descripcion,
	    E objeto, ImageIcon icono) {
	super(descripcion,objeto);
	_icono = icono;
    }

    public Icon getIcono() {
	return _icono;
    }

    public void setIcono(Icon icono) {
	_icono = icono;
    }

    public SelectableNodeWithIcon<E> clone(){
	return new SelectableNodeWithIcon<E>(getDescripcion(), getObject(), getSeleccionado(), _icono);
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