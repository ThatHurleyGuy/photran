/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Dennis Lin (TODO COMPANY NAME) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.tests.refactoring;

import junit.framework.Test;

import org.eclipse.core.resources.IFile;
import org.eclipse.photran.internal.core.refactoring.IntroduceInterfaceRefactoring;
import org.eclipse.photran.internal.tests.Activator;
import org.eclipse.photran.internal.tests.PhotranRefactoringTestSuiteFromMarkers;

/**
 * Test suite for moving interface refactoring
 * @author Dennis Lin
 */
public class IntroduceInterfaceRefactoringTestSuite extends
    PhotranRefactoringTestSuiteFromMarkers<IntroduceInterfaceRefactoring>
{
    private static final String DIR = "refactoring-test-code/introduce-interface";

    public static Test suite() throws Exception
    {
        return new IntroduceInterfaceRefactoringTestSuite();
    }

    public IntroduceInterfaceRefactoringTestSuite() throws Exception
    {
        super(Activator.getDefault(), "Running Move Interface refactoring in", DIR,
            IntroduceInterfaceRefactoring.class);
    }

    @Override
    protected boolean shouldCompile(IFile fileContainingMarker)
    {
        String name = fileContainingMarker.getName();
        if (name.equalsIgnoreCase("test-complex.f90"))
           return false; 
        if (name.equalsIgnoreCase("test-fail.f90"))
            return false;
        return true;
    }
}
