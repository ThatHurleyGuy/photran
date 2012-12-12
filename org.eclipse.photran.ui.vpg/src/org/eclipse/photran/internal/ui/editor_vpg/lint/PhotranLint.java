/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    seanhurley (TODO COMPANY NAME) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.lint;

/**
 * 
 * @author seanhurley
 */
public class PhotranLint
{
    // This attribute is used to set what the type of the problem is
    public static final String PHOTRAN_LINT_ATTRIBUTE = "PhotranLintProblem";

    // This value should be set during the IMarker creation to correspond to the proper quick fix
    public static final int UNSAFE_CALL_VALUE = 0;
}
