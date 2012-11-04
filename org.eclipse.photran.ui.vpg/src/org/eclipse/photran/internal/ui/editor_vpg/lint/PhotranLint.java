/*******************************************************************************
 * Copyright (c) 2012 UIUC and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Sean Hurley (UIUC) - Initial API and implementation
 *    Chase Geigle (UIUC) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.lint;

/**
 * 
 * 
 */
public class PhotranLint
{
    // This attribute is used to set what the type of the problem is
    // Note that we are using this instead of the Messages.java since these really don't need to be
    // externalized and having them straight in the code is a reasonable place in this case.
    public static final String PHOTRAN_LINT_ATTRIBUTE = "PhotranLintProblem"; //$NON-NLS-1$

    public static final String PHOTRAN_LINT_EXTRA = "PhotranLintExtra"; //$NON-NLS-1$

    // This value should be set during the IMarker creation to correspond to the proper quick fix
    public static final int UNSAFE_CALL_VALUE = 0;
}
