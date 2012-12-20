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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.RefactoringStatusEntry;
import org.eclipse.photran.internal.core.refactoring.IntroduceInterfaceRefactoring;
import org.eclipse.photran.internal.ui.editor_vpg.Messages;
import org.eclipse.rephraserengine.ui.UIUtil;
import org.eclipse.ui.IMarkerResolution;

/**
 * This is the resolution for the unsafe calls refactoring. This will be called to fix an issue
 * where the unsafe call has been made and we need to generate an interface. To do this we call the
 * {@link IntroduceInterfaceRefactoring} to do the work for us.
 */
public class TypesafeCallQuickFixer implements IMarkerResolution
{
    private String label;

    protected static String EOL = System.getProperty("line.separator"); //$NON-NLS-1$

    public TypesafeCallQuickFixer(IMarker marker)
    {
        this.label = Messages.FortranEditorTasks_Lint_QuickFixUnsafeCall;
    }

    public String getLabel()
    {
        return label;
    }

    public void run(IMarker marker)
    {
        callRefactoring(marker);
    }

    public void callRefactoring(IMarker marker)
    {
        // Ask the user to save files
        if (!UIUtil.askUserToSaveModifiedFiles()) { return; }

        IntroduceInterfaceRefactoring refactoring = new IntroduceInterfaceRefactoring();

        // We give it a selection of length zero since the refactoring only needs to know where to
        // token starts
        TextSelection selection = new TextSelection(marker.getAttribute(IMarker.CHAR_START, 0), 0);
        refactoring.initialize((IFile)marker.getResource(), selection);
        RefactoringStatus status = refactoring.checkInitialConditions(new NullProgressMonitor());

        // Ensure nothing went wrong
        if (!checkStatusOK(refactoring, status)) { return; }
        status = refactoring.checkFinalConditions(new NullProgressMonitor());

        // Again, check that nothing went wrong
        if (!checkStatusOK(refactoring, status)) { return; }

        Change change;
        try
        {
            // Commit the change to the file
            change = refactoring.createChange(new NullProgressMonitor());
            change.perform(new NullProgressMonitor());
        }
        catch (OperationCanceledException e)
        {
            e.printStackTrace();
        }
        catch (CoreException e)
        {
            e.printStackTrace();
        }
    }

    private boolean checkStatusOK(IntroduceInterfaceRefactoring refactoring,
        RefactoringStatus status)
    {
        if (!status.isOK())
        {
            RefactoringStatusEntry[] entries = status.getEntries();
            MessageDialog.openInformation(null, refactoring.getName(),
                entries[entries.length - 1].getMessage());
            return false;
        }

        return true;
    }
}
