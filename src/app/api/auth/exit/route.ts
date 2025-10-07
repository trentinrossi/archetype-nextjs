// src/app/api/auth/exit/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { ExitRequestDTO, ExitResponseDTO } from '@/types/user-security';

export async function POST(request: NextRequest) {
  try {
    const body: ExitRequestDTO = await request.json();

    // Validate request body
    if (!body.userId || !body.sessionId) {
      return NextResponse.json(
        {
          success: false,
          message: 'Invalid exit request - missing required fields',
          timestamp: new Date().toISOString()
        },
        { status: 400 }
      );
    }

    // In production, you would:
    // 1. Validate the session token
    // 2. Invalidate the session in the database
    // 3. Log the exit event for audit purposes
    // 4. Clean up any temporary resources

    // For now, we'll simulate a successful exit
    const response: ExitResponseDTO = {
      success: true,
      message: 'Thank you for using CardDemo System. Have a nice day!',
      timestamp: new Date().toISOString()
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Exit error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error during exit',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}