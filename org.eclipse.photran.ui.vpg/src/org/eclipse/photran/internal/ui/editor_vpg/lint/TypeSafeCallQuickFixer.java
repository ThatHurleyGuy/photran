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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.RefactoringStatusEntry;
import org.eclipse.photran.core.IFortranAST;
import org.eclipse.photran.internal.core.FortranAST;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.analysis.binding.ScopingNode;
import org.eclipse.photran.internal.core.lexer.ASTLexerFactory;
import org.eclipse.photran.internal.core.lexer.IAccumulatingLexer;
import org.eclipse.photran.internal.core.parser.ASTEntityDeclNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionParNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionStmtNode;
import org.eclipse.photran.internal.core.parser.ASTMainProgramNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineParNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineStmtNode;
import org.eclipse.photran.internal.core.parser.ASTTypeDeclarationStmtNode;
import org.eclipse.photran.internal.core.parser.ASTVisitor;
import org.eclipse.photran.internal.core.parser.IASTListNode;
import org.eclipse.photran.internal.core.parser.IBodyConstruct;
import org.eclipse.photran.internal.core.parser.Parser;
import org.eclipse.photran.internal.core.refactoring.CreateInterfaceRefactoring;
import org.eclipse.photran.internal.core.refactoring.IntroduceInterfaceRefactoring;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;
import org.eclipse.photran.internal.ui.editor.FortranEditor;
import org.eclipse.photran.internal.ui.editor_vpg.Messages;
import org.eclipse.rephraserengine.ui.UIUtil;
import org.eclipse.ui.IMarkerResolution;

/**
 * 
 * @author seanhurley
 */
public class TypeSafeCallQuickFixer implements IMarkerResolution
{
    private String label;

    protected static String EOL = System.getProperty("line.separator"); //$NON-NLS-1$

    public TypeSafeCallQuickFixer(IMarker marker)
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

    private String readStream(InputStream inputStream) throws IOException
    {
        StringBuffer sb = new StringBuffer(4096);
        BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
        for (int offset = 0, ch = in.read(); ch >= 0; ch = in.read())
        {
            sb.append((char)ch);
            offset++;
        }
        in.close();
        return sb.toString();
    }

    public void callRefactoring(IMarker marker)
    {

        if (!UIUtil.askUserToSaveModifiedFiles()) return;

        IntroduceInterfaceRefactoring refactoring = new IntroduceInterfaceRefactoring();
        TextSelection selection = new TextSelection(marker.getAttribute(IMarker.CHAR_START, 0), 0);
        refactoring.initialize((IFile)marker.getResource(), selection);
        RefactoringStatus status = refactoring.checkInitialConditions(new NullProgressMonitor());
        if (!status.isOK())
        {
            RefactoringStatusEntry[] entries = status.getEntries();
            MessageDialog.openInformation(null, refactoring.getName(),
                entries[entries.length - 1].getMessage());
            return;
        }
        status = refactoring.checkFinalConditions(new NullProgressMonitor());
        if (!status.isOK())
        {
            RefactoringStatusEntry[] entries = status.getEntries();
            MessageDialog.openInformation(null, refactoring.getName(),
                entries[entries.length - 1].getMessage());
            return;
        }

        Change change;
        try
        {
            change = refactoring.createChange(new NullProgressMonitor());
            change.perform(new NullProgressMonitor());
        }
        catch (OperationCanceledException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (CoreException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
