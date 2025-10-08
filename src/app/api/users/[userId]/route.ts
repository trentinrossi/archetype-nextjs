// src/api/users/[userId]/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, UpdateUserSecurityRequest } from '@/types/userSecurity';

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

export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const user = mockUsers.find(u => u.userId === userId);

    if (!user) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_NOT_FOUND',
        errorMessage: 'User ID NOT found...'
      }, { status: 404 });
    }

    return NextResponse.json({
      success: true,
      data: user
    }, { status: 200 });

  } catch (error) {
    console.error('Get user error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Unable to lookup User...'
    }, { status: 500 });
  }
}

export async function PUT(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const body: UpdateUserSecurityRequest = await request.json();
    
    const userIndex = mockUsers.findIndex(u => u.userId === userId);
    
    if (userIndex === -1) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_NOT_FOUND',
        errorMessage: 'User ID NOT found...'
      }, { status: 404 });
    }

    // Validate required fields if provided (COUSR02C business rules)
    if (body.firstName !== undefined && (!body.firstName || body.firstName.trim() === '')) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'First Name can NOT be empty...'
      }, { status: 400 });
    }

    if (body.lastName !== undefined && (!body.lastName || body.lastName.trim() === '')) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'Last Name can NOT be empty...'
      }, { status: 400 });
    }

    if (body.userType !== undefined && (!body.userType || body.userType.trim() === '')) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'User Type can NOT be empty...'
      }, { status: 400 });
    }

    const currentUser = mockUsers[userIndex];
    let hasChanges = false;

    // Check for modifications (COUSR02C business rule)
    if (body.firstName && body.firstName !== currentUser.firstName) {
      currentUser.firstName = body.firstName;
      hasChanges = true;
    }

    if (body.lastName && body.lastName !== currentUser.lastName) {
      currentUser.lastName = body.lastName;
      hasChanges = true;
    }

    if (body.userType && body.userType !== currentUser.userType) {
      currentUser.userType = body.userType;
      hasChanges = true;
    }

    if (body.userName && body.userName !== currentUser.userName) {
      currentUser.userName = body.userName;
      hasChanges = true;
    }

    if (body.email && body.email !== currentUser.email) {
      currentUser.email = body.email;
      hasChanges = true;
    }

    if (body.isActive !== undefined && body.isActive !== currentUser.isActive) {
      currentUser.isActive = body.isActive;
      hasChanges = true;
    }

    if (!hasChanges) {
      return NextResponse.json({
        success: false,
        errorCode: 'NO_CHANGES',
        errorMessage: 'Please modify to update...'
      }, { status: 400 });
    }

    // Update modification timestamp
    currentUser.modifiedDate = new Date();
    currentUser.modifiedBy = 'SYSTEM'; // Should be current user

    mockUsers[userIndex] = currentUser;

    return NextResponse.json({
      success: true,
      message: `User ${userId} has been updated...`,
      data: currentUser
    }, { status: 200 });

  } catch (error) {
    console.error('Update user error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Unable to Update User...'
    }, { status: 500 });
  }
}

export async function DELETE(
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
        errorMessage: 'User ID NOT found...'
      }, { status: 404 });
    }

    // Remove user (COUSR03C business rule)
    mockUsers.splice(userIndex, 1);

    return NextResponse.json({
      success: true,
      message: `User ${userId} has been deleted...`
    }, { status: 204 });

  } catch (error) {
    console.error('Delete user error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Unable to Update User...'
    }, { status: 500 });
  }
}