// src/api/users/[userId]/deactivate/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO } from '@/types/userSecurity';

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

export async function PATCH(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const userIndex = mockUsers.findIndex(u => u.userId === userId);

    if (userIndex === -1) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_NOT_FOUND',
        errorMessage: 'User not found'
      }, { status: 404 });
    }

    const user = mockUsers[userIndex];
    
    // Deactivate user
    user.isActive = false;
    user.modifiedDate = new Date();
    user.modifiedBy = 'SYSTEM'; // Should be current user

    mockUsers[userIndex] = user;

    return NextResponse.json({
      success: true,
      message: `User ${userId} deactivated successfully`,
      data: user
    }, { status: 200 });

  } catch (error) {
    console.error('Deactivate user error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    }, { status: 500 });
  }
}