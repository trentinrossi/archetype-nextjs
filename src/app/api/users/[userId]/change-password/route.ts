// src/api/users/[userId]/change-password/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, ChangePasswordRequest } from '@/types/userSecurity';

// Mock data - replace with actual database
let mockUsers: UserSecurityDTO[] = [
  {
    userId: 'ADMIN001',
    userName: 'admin001',
    userType: 'ADMIN',
    email: 'admin@carddemo.com',
    firstName: 'System',
    lastName: 'Administrator',
    isActive: true,
    failedAttempts: 0,
    isLocked: false,
    createdDate: new Date('2024-01-01'),
    modifiedDate: new Date('2024-01-01'),
    createdBy: 'SYSTEM',
    modifiedBy: 'SYSTEM'
  },
  {
    userId: 'USER001',
    userName: 'user001',
    userType: 'GENERAL',
    email: 'user@carddemo.com',
    firstName: 'General',
    lastName: 'User',
    isActive: true,
    failedAttempts: 0,
    isLocked: false,
    createdDate: new Date('2024-01-02'),
    modifiedDate: new Date('2024-01-02'),
    createdBy: 'ADMIN001',
    modifiedBy: 'ADMIN001'
  }
];

// Mock password storage - replace with actual secure storage
const mockPasswords: { [userId: string]: string } = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123'
};

export async function PATCH(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const body: Omit<ChangePasswordRequest, 'userId'> = await request.json();
    
    const userIndex = mockUsers.findIndex(u => u.userId === userId);

    if (userIndex === -1) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_NOT_FOUND',
        errorMessage: 'User not found'
      }, { status: 404 });
    }

    // Validate required fields
    if (!body.newPassword || body.newPassword.trim() === '') {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'New password cannot be empty'
      }, { status: 400 });
    }

    // Validate password length
    if (body.newPassword.length > 8) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'Password cannot exceed 8 characters'
      }, { status: 400 });
    }

    // Validate current password if provided
    if (body.currentPassword) {
      const currentPassword = mockPasswords[userId];
      if (currentPassword && body.currentPassword !== currentPassword) {
        return NextResponse.json({
          success: false,
          errorCode: 'INVALID_CREDENTIALS',
          errorMessage: 'Current password is incorrect'
        }, { status: 400 });
      }
    }

    // Validate password confirmation if provided
    if (body.confirmPassword && body.newPassword !== body.confirmPassword) {
      return NextResponse.json({
        success: false,
        errorCode: 'PASSWORD_MISMATCH',
        errorMessage: 'Password confirmation does not match'
      }, { status: 400 });
    }

    // Update password
    mockPasswords[userId] = body.newPassword;
    
    // Update user modification timestamp
    const user = mockUsers[userIndex];
    user.modifiedDate = new Date();
    user.modifiedBy = 'SYSTEM'; // Should be current user
    mockUsers[userIndex] = user;

    return NextResponse.json({
      success: true,
      message: `Password changed successfully for user ${userId}`
    }, { status: 200 });

  } catch (error) {
    console.error('Change password error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    }, { status: 500 });
  }
}