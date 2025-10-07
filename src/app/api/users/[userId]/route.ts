// src/app/api/users/[userId]/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, UpdateUserSecurityRequest } from '@/types/user-security';

// Mock user database - same as in main users route
let mockUsers: Record<string, UserSecurityDTO> = {
  'ADMIN001': {
    userId: 'ADMIN001',
    userName: 'System Administrator',
    userType: 'ADMIN',
    userStatus: 'ACTIVE',
    passwordExpired: false,
    failedSignonAttempts: 0,
    maxFailedAttempts: 3,
    accountLocked: false,
    securityLevel: 9,
    userGroup: 'ADMINISTRATORS',
    department: 'IT',
    email: 'admin@carddemo.com',
    createdDate: '2024-01-01',
    createdTime: '09:00:00',
    createdBy: 'SYSTEM',
    profileData: {
      firstName: 'System',
      lastName: 'Administrator',
      displayName: 'Admin',
      title: 'System Administrator'
    },
    permissions: [],
    roles: []
  },
  'USER001': {
    userId: 'USER001',
    userName: 'General User',
    userType: 'USER',
    userStatus: 'ACTIVE',
    passwordExpired: false,
    failedSignonAttempts: 0,
    maxFailedAttempts: 3,
    accountLocked: false,
    securityLevel: 1,
    userGroup: 'USERS',
    department: 'GENERAL',
    email: 'user@carddemo.com',
    createdDate: '2024-01-01',
    createdTime: '09:00:00',
    createdBy: 'ADMIN001',
    profileData: {
      firstName: 'General',
      lastName: 'User',
      displayName: 'User',
      title: 'General User'
    },
    permissions: [],
    roles: []
  }
};

let mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123'
};

// GET /api/users/[userId] - Get user by ID
export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const user = mockUsers[userId];

    if (!user) {
      return NextResponse.json(
        {
          error: 'Not Found',
          message: 'User not found',
          statusCode: 404,
          timestamp: new Date().toISOString(),
          path: `/api/users/${params.userId}`
        },
        { status: 404 }
      );
    }

    return NextResponse.json(user, { status: 200 });

  } catch (error) {
    console.error('Get user error:', error);
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: 'Failed to retrieve user',
        statusCode: 500,
        timestamp: new Date().toISOString(),
        path: `/api/users/${params.userId}`
      },
      { status: 500 }
    );
  }
}

// PUT /api/users/[userId] - Update user
export async function PUT(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const body: UpdateUserSecurityRequest = await request.json();

    // Check if user exists
    const existingUser = mockUsers[userId];
    if (!existingUser) {
      return NextResponse.json(
        {
          error: 'Not Found',
          message: 'User not found',
          statusCode: 404,
          timestamp: new Date().toISOString(),
          path: `/api/users/${params.userId}`
        },
        { status: 404 }
      );
    }

    // Update user fields
    const now = new Date();
    const updatedUser: UserSecurityDTO = {
      ...existingUser,
      ...(body.userName && { userName: body.userName }),
      ...(body.userType && { userType: body.userType }),
      ...(body.userStatus && { userStatus: body.userStatus }),
      ...(body.securityLevel !== undefined && { securityLevel: body.securityLevel }),
      ...(body.userGroup && { userGroup: body.userGroup }),
      ...(body.department && { department: body.department }),
      ...(body.email && { email: body.email }),
      ...(body.phoneNumber && { phoneNumber: body.phoneNumber }),
      ...(body.maxFailedAttempts !== undefined && { maxFailedAttempts: body.maxFailedAttempts }),
      ...(body.resetFailedAttempts && { failedSignonAttempts: 0 }),
      ...(body.unlockAccount && { accountLocked: false, lockoutDate: undefined, lockoutTime: undefined }),
      ...(body.extendPasswordExpiry && { passwordExpired: false, passwordExpiryDate: undefined }),
      lastModifiedDate: now.toISOString().split('T')[0],
      lastModifiedTime: now.toTimeString().split(' ')[0],
      lastModifiedBy: 'SYSTEM', // In production, this would be the current user
      ...(body.profileData && { 
        profileData: { 
          ...existingUser.profileData, 
          ...body.profileData 
        } 
      })
    };

    // Store updated user
    mockUsers[userId] = updatedUser;

    return NextResponse.json(updatedUser, { status: 200 });

  } catch (error) {
    console.error('Update user error:', error);
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: 'Failed to update user',
        statusCode: 500,
        timestamp: new Date().toISOString(),
        path: `/api/users/${params.userId}`
      },
      { status: 500 }
    );
  }
}

// DELETE /api/users/[userId] - Delete user
export async function DELETE(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();

    // Check if user exists
    if (!mockUsers[userId]) {
      return NextResponse.json(
        {
          error: 'Not Found',
          message: 'User not found',
          statusCode: 404,
          timestamp: new Date().toISOString(),
          path: `/api/users/${params.userId}`
        },
        { status: 404 }
      );
    }

    // Delete user and password
    delete mockUsers[userId];
    delete mockPasswords[userId];

    return NextResponse.json(null, { status: 204 });

  } catch (error) {
    console.error('Delete user error:', error);
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: 'Failed to delete user',
        statusCode: 500,
        timestamp: new Date().toISOString(),
        path: `/api/users/${params.userId}`
      },
      { status: 500 }
    );
  }
}