/*******************************************************************************
 * Copyright (c) 2012 UIUC and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * This file is based on the AnnotationHover from the actual eclipse project
 * http://grepcode.com/file/repository.grepcode.com/java/eclipse.org/3.5/org.eclipse.pde/ui/3.5.0/org/eclipse/pde/internal/ui/editor/text/AnnotationHover.java
 *
 * Contributors:
 *    Sean Hurley (UIUC) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.hover;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.texteditor.MarkerAnnotation;

public class AnnotationHover implements IAnnotationHover
{

    private static String EOL = System.getProperty("line.separator"); //$NON-NLS-1$

    public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber)
    {
        ArrayList<String> messages = getMessagesForLine(sourceViewer, lineNumber);

        if (messages.size() == 0) { return null; }

        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < messages.size(); i++)
        {
            builder.append(messages.get(i));
            if (i < messages.size() - 1)
            {
                builder.append(EOL);
            }
        }
        return builder.toString();
    }

    private ArrayList<String> getMessagesForLine(ISourceViewer viewer, int line)
    {
        IDocument document = viewer.getDocument();
        IAnnotationModel model = viewer.getAnnotationModel();
        ArrayList<String> messages = new ArrayList<String>();

        if (model == null) { return messages; }

        @SuppressWarnings("unchecked")
        Iterator<Annotation> iter = model.getAnnotationIterator();
        while (iter.hasNext())
        {
            Annotation annotation = iter.next();
            if (annotation instanceof MarkerAnnotation)
            {
                MarkerAnnotation markerAnnotation = (MarkerAnnotation)annotation;
                if (compareRulerLine(model.getPosition(markerAnnotation), document, line))
                {
                    String message = markerAnnotation.getText();
                    if (message != null && message.trim().length() > 0)
                    {
                        messages.add(message);
                    }
                }
            }
        }
        return messages;
    }

    private boolean compareRulerLine(Position position, IDocument document, int line)
    {
        try
        {
            if (position.getOffset() >= 0 && position.getLength() >= 0) { return document
                .getLineOfOffset(position.getOffset()) == line; }
        }
        catch (BadLocationException e)
        {
        }
        return false;
    }
}