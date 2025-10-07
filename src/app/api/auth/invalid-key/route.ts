// src/app/api/auth/invalid-key/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { InvalidKeyRequestDTO, InvalidKeyResponseDTO } from '@/types/user-security';

export async function POST(request: NextRequest) {
  try {
    const body: InvalidKeyRequestDTO = await request.json();

    // Validate request body
    if (!body.userId || !body.invalidKey) {
      return NextResponse.json(
        {
          acknowledged: false,
          message: 'Invalid request - missing required fields',
          timestamp: new Date().toISOString()
        },
        { status: 400 }
      );
    }

    // In production, you would:
    // 1. Log the invalid key press for security monitoring
    // 2. Check if this is a pattern of suspicious activity
    // 3. Potentially implement rate limiting or account protection

    // Map common invalid keys to user-friendly messages
    const keyMessages: Record<string, string> = {
      'F1': 'Help function not available in this context',
      'F2': 'Function key F2 is not assigned',
      'F6': 'Function key F6 is not assigned',
      'F9': 'Function key F9 is not assigned',
      'F10': 'Function key F10 is not assigned',
      'F11': 'Function key F11 is not assigned',
      'CTRL+C': 'Copy function not available',
      'CTRL+V': 'Paste function not available',
      'ALT+TAB': 'Task switching not allowed',
      'ESC': 'Escape key not functional in this screen'
    };

    const specificMessage = keyMessages[body.invalidKey.toUpperCase()] || 
                           `Invalid key pressed: ${body.invalidKey}`;

    const response: InvalidKeyResponseDTO = {
      acknowledged: true,
      message: `INVALID KEY PRESSED. ${specificMessage}. Please use only assigned function keys.`,
      timestamp: new Date().toISOString(),
      actionTaken: 'Key press ignored, user notified'
    };

    return NextResponse.json(response, { status: 400 });

  } catch (error) {
    console.error('Invalid key handling error:', error);
    return NextResponse.json(
      {
        acknowledged: false,
        message: 'Internal server error handling invalid key',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}