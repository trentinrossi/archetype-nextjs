import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, ChangePasswordRequest } from '@/types/user';

// Mock user database - in real implementation, this would be a database
let mockUsers: UserSecurityDTO[] = [
  {
    userId: 'ADMIN001',
    password: 'ADMIN123',
    userType: 'ADMIN',
    programName: 'COSGN00C',
    transactionId: 'CC00',
    active: true,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
    userTypeDisplayName: 'Administrator',
    redirectProgram: 'COADM01C',
    canAuthenticate: true
  },
  {
    userId: 'USER001',
    password: 'USER123',
    userType: 'GENERAL',
    programName: 'COSGN00C',
    transactionId: 'CC00',
    active: true,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
    userTypeDisplayName: 'General User',
    redirectProgram: 'COMEN01C',
    canAuthenticate: true
  }
];

// PATCH /api/users/[userId]/change-password - Change user password
export async function PATCH(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const body: ChangePasswordRequest = await request.json();

    if (!userId || userId.length > 8) {
      return NextResponse.json(
        { error: 'Invalid user ID' },
        { status: 400 }
      );
    }

    if (!body.newPassword || body.newPassword.trim() === '') {
      return NextResponse.json(
        { error: 'New password is required' },
        { status: 400 }
      );
    }

    if (body.newPassword.length > 8) {
      return NextResponse.json(
        { error: 'Password must be 8 characters or less' },
        { status: 400 }
      );
    }

    const userIndex = mockUsers.findIndex(u => u.userId === userId);
    
    if (userIndex === -1) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      );
    }

    // Update password
    mockUsers[userIndex] = {
      ...mockUsers[userIndex],
      password: body.newPassword,
      updatedAt: new Date().toISOString()
    };

    return NextResponse.json({
      success: true,
      message: `Password changed successfully for user ${userId}`
    });

  } catch (error) {
    console.error('Change password error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}