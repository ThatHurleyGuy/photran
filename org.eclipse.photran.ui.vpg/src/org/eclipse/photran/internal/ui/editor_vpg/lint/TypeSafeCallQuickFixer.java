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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IMarkerResolution;

/**
 * 
 * @author seanhurley
 */
public class TypeSafeCallQuickFixer implements IMarkerResolution
    {
    private String label;

    public TypeSafeCallQuickFixer(IMarker marker)
    {
        this.label = "Quick fix number 1";
    }

    public String getLabel()
    {
        return label;
    }

    public void run(IMarker marker)
    {
        // TODO Tie this in to the actual GUI
        MessageDialog.openInformation(null, "QuickFix Demo",
            "This quick-fix is not yet implemented");
    }
}
