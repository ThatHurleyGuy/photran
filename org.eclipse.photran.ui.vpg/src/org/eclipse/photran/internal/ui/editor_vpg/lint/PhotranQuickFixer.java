/*******************************************************************************
 * Copyright (c) 2012 UIUC and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Sean Hurley (UIUC) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.lint;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

/**
 * This will be called when the eclipse API needs to get possible quick fixes for a particular issue
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
            e.printStackTrace();
        }

        // To add a new possible solution, just create the marker with a different attribute and add
        // it to the switch cases
        switch (problem)
        {
            case PhotranLint.UNSAFE_CALL_VALUE:
                return new IMarkerResolution[] { new TypesafeCallQuickFixer(mk), };

            default:
                break;
        }

        return null;
    }
}
