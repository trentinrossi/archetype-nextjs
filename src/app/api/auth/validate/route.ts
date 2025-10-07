import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO } from '@/types/user';

export async function POST(request: NextRequest) {
  try {
    // Check if user has valid session
    const sessionCookie = request.cookies.get('user-session');
    
    if (!sessionCookie) {
      return NextResponse.json(
        {
          valid: false,
          message: 'No active session',
        },
        { status: 401 }
      );
    }

    try {
      const sessionData = JSON.parse(sessionCookie.value);
      
      // Check if session is expired (30 minutes)
      const loginTime = new Date(sessionData.loginTime);
      const now = new Date();
      const diffMinutes = (now.getTime() - loginTime.getTime()) / (1000 * 60);
      
      if (diffMinutes > 30) {
        return NextResponse.json(
          {
            valid: false,
            message: 'Session expired',
          },
          { status: 401 }
        );
      }

      return NextResponse.json(
        {
          valid: true,
          message: 'Session is valid',
          user: {
            userId: sessionData.userId,
            userType: sessionData.userType,
          },
        },
        { status: 200 }
      );

    } catch (parseError) {
      return NextResponse.json(
        {
          valid: false,
          message: 'Invalid session data',
        },
        { status: 401 }
      );
    }

  } catch (error) {
    console.error('Session validation error:', error);
    return NextResponse.json(
      {
        valid: false,
        message: 'Internal server error',
      },
      { status: 500 }
    );
  }
}

// Also support validation via request body for credential checking
export async function PUT(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();

    // Validate input format without authentication
    const errors: string[] = [];

    if (!body.userId || body.userId.trim() === '') {
      errors.push('User ID is required');
    } else if (body.userId.length > 8) {
      errors.push('User ID must be 8 characters or less');
    }

    if (!body.password || body.password.trim() === '') {
      errors.push('Password is required');
    } else if (body.password.length > 8) {
      errors.push('Password must be 8 characters or less');
    }

    if (errors.length > 0) {
      return NextResponse.json(
        {
          valid: false,
          message: 'Validation failed',
          errors,
        },
        { status: 400 }
      );
    }

    return NextResponse.json(
      {
        valid: true,
        message: 'Credentials format is valid',
      },
      { status: 200 }
    );

  } catch (error) {
    console.error('Credential validation error:', error);
    return NextResponse.json(
      {
        valid: false,
        message: 'Internal server error',
      },
      { status: 500 }
    );
  }
}