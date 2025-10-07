import { NextRequest, NextResponse } from 'next/server';
import { ChangePasswordRequest } from '@/types/user';

// Mock password storage - in real implementation this would be hashed
const mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123',
};

function checkAuthentication(request: NextRequest) {
  const sessionCookie = request.cookies.get('user-session');
  
  if (!sessionCookie) {
    return null;
  }

  try {
    const sessionData = JSON.parse(sessionCookie.value);
    
    // Check if session is expired (30 minutes)
    const loginTime = new Date(sessionData.loginTime);
    const now = new Date();
    const diffMinutes = (now.getTime() - loginTime.getTime()) / (1000 * 60);
    
    if (diffMinutes > 30) {
      return null;
    }

    return sessionData;
  } catch {
    return null;
  }
}

// PATCH /api/users/[userId]/change-password - Change user password
export async function PATCH(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    // Check authentication
    const session = checkAuthentication(request);
    if (!session) {
      return NextResponse.json(
        { message: 'Authentication required' },
        { status: 401 }
      );
    }

    const userId = params.userId.toUpperCase();
    const body: { newPassword: string } = await request.json();

    // Check if user is admin or changing their own password
    if (session.userType !== 'A' && session.userId !== userId) {
      return NextResponse.json(
        { message: 'Access denied' },
        { status: 403 }
      );
    }

    // Validate new password
    if (!body.newPassword || body.newPassword.trim() === '') {
      return NextResponse.json(
        { message: 'Password can NOT be empty...' },
        { status: 400 }
      );
    }

    if (body.newPassword.length > 8) {
      return NextResponse.json(
        { message: 'Password must be 8 characters or less' },
        { status: 400 }
      );
    }

    // Check if user exists
    if (!mockPasswords[userId]) {
      return NextResponse.json(
        { message: 'User ID NOT found...' },
        { status: 404 }
      );
    }

    // Update password
    mockPasswords[userId] = body.newPassword;

    return NextResponse.json(
      { message: `Password for user ${userId} has been changed...` },
      { status: 200 }
    );

  } catch (error) {
    console.error('Change password error:', error);
    return NextResponse.json(
      { message: 'Unable to change password...' },
      { status: 500 }
    );
  }
}