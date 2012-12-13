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

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

/**
 * 
 * @author seanhurley
 */
public class PhotranQuickFixer implements IMarkerResolutionGenerator
{
    public IMarkerResolution[] getResolutions(IMarker mk)
    {
        int problem = -1;
        try
        {
            problem = (Integer)mk.getAttribute(PhotranLint.PHOTRAN_LINT_ATTRIBUTE);
        }
        catch (CoreException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        switch (problem)
        {
            case PhotranLint.UNSAFE_CALL_VALUE:
                return new IMarkerResolution[] { new TypeSafeCallQuickFixer(mk), };

            default:
                break;
        }

        return null;
    }
}
