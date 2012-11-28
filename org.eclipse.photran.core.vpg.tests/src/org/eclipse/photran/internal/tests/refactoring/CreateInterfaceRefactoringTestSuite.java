/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    DFL (TODO COMPANY NAME) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.tests.refactoring;

import junit.framework.Test;

import org.eclipse.core.resources.IFile;
import org.eclipse.photran.internal.core.refactoring.UnrollLoopRefactoring;
import org.eclipse.photran.internal.tests.Activator;
import org.eclipse.photran.internal.tests.PhotranRefactoringTestSuiteFromMarkers;

/**
 * Test suite for moving interface refactoring
 * @author Dennis Lin
 */
public class CreateInterfaceRefactoringTestSuite extends PhotranRefactoringTestSuiteFromMarkers<CreateInterfaceRefactoring>
{
    private static final String DIR = "refactoring-test-code/create-interface";
    public static Test suite() throws Exception
    {
        return new UnrollLoopRefactoringTestSuite();
    }

    public CreateInterfaceRefactoringTestSuite() throws Exception
    {
        super(Activator.getDefault(),
            "Running Move Interface refactoring in",
            DIR,
            CreateInterfaceRefactoring.class);
    }

    @Override protected boolean shouldCompile(IFile fileContainingMarker)
    {
        String name = fileContainingMarker.getName();
        return
            !name.equalsIgnoreCase("test-fail.f90");
    }
    
}
