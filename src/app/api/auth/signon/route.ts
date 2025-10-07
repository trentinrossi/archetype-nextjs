import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, SignonResponseDTO, UserSecurityDTO } from '@/types/user';

// Mock user database - in real implementation this would be a database
const mockUsers: UserSecurityDTO[] = [
  {
    userId: 'ADMIN001',
    firstName: 'System',
    lastName: 'Administrator',
    userType: 'A',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  },
  {
    userId: 'USER001',
    firstName: 'General',
    lastName: 'User',
    userType: 'G',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  },
];

// Mock password storage - in real implementation this would be hashed
const mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123',
};

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();

    // Validate input
    if (!body.userId || body.userId.trim() === '') {
      return NextResponse.json(
        {
          success: false,
          message: 'Please enter User ID',
          errorCode: 'VALIDATION_ERROR',
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    if (!body.password || body.password.trim() === '') {
      return NextResponse.json(
        {
          success: false,
          message: 'Please enter Password',
          errorCode: 'VALIDATION_ERROR',
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    // Validate field lengths (COBOL constraints)
    if (body.userId.length > 8) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID must be 8 characters or less',
          errorCode: 'VALIDATION_ERROR',
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    if (body.password.length > 8) {
      return NextResponse.json(
        {
          success: false,
          message: 'Password must be 8 characters or less',
          errorCode: 'VALIDATION_ERROR',
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    // Convert to uppercase for case-insensitive lookup (COBOL behavior)
    const userId = body.userId.toUpperCase().trim();
    
    // Find user
    const user = mockUsers.find(u => u.userId === userId);
    
    if (!user) {
      return NextResponse.json(
        {
          success: false,
          message: 'User not found',
          errorCode: 'USER_NOT_FOUND',
        } as SignonResponseDTO,
        { status: 401 }
      );
    }

    // Check password
    const storedPassword = mockPasswords[userId];
    if (!storedPassword || storedPassword !== body.password) {
      return NextResponse.json(
        {
          success: false,
          message: 'Wrong Password',
          errorCode: 'INVALID_PASSWORD',
        } as SignonResponseDTO,
        { status: 401 }
      );
    }

    // Determine redirect program based on user type (COBOL business logic)
    const redirectProgram = user.userType === 'A' ? 'COADM01C' : 'COMEN01C';

    // Create session (in real implementation, use proper session management)
    const sessionData = {
      userId: user.userId,
      userType: user.userType,
      loginTime: new Date().toISOString(),
    };

    const response = NextResponse.json(
      {
        success: true,
        user,
        redirectProgram,
        message: 'Authentication successful',
      } as SignonResponseDTO,
      { status: 200 }
    );

    // Set session cookie (in real implementation, use secure session management)
    response.cookies.set('user-session', JSON.stringify(sessionData), {
      httpOnly: true,
      secure: process.env.NODE_ENV === 'production',
      sameSite: 'strict',
      maxAge: 30 * 60, // 30 minutes
    });

    return response;

  } catch (error) {
    console.error('Signon error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error',
        errorCode: 'SERVER_ERROR',
      } as SignonResponseDTO,
      { status: 500 }
    );
  }
}