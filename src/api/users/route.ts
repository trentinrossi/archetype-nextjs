// src/api/users/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, CreateUserSecurityRequest, UserSecurityListResponse } from '@/types/userSecurity';

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

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = parseInt(searchParams.get('page') || '0');
    const size = parseInt(searchParams.get('size') || '20');
    const sort = searchParams.get('sort') || 'userId';

    // Apply pagination
    const startIndex = page * size;
    const endIndex = startIndex + size;
    const paginatedUsers = mockUsers.slice(startIndex, endIndex);

    const response: UserSecurityListResponse = {
      users: paginatedUsers,
      totalCount: mockUsers.length,
      page,
      pageSize: size,
      totalPages: Math.ceil(mockUsers.length / size)
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Get users error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    }, { status: 500 });
  }
}

export async function POST(request: NextRequest) {
  try {
    const body: CreateUserSecurityRequest = await request.json();
    
    // Validate required fields (COUSR01C business rules)
    if (!body.firstName || body.firstName.trim() === '') {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'First Name can NOT be empty...'
      }, { status: 400 });
    }

    if (!body.lastName || body.lastName.trim() === '') {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'Last Name can NOT be empty...'
      }, { status: 400 });
    }

    if (!body.userId || body.userId.trim() === '') {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'User ID can NOT be empty...'
      }, { status: 400 });
    }

    if (!body.password || body.password.trim() === '') {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'Password can NOT be empty...'
      }, { status: 400 });
    }

    if (!body.userType || body.userType.trim() === '') {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'User Type can NOT be empty...'
      }, { status: 400 });
    }

    // Validate field lengths
    if (body.userId.length > 8) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'User ID cannot exceed 8 characters'
      }, { status: 400 });
    }

    if (body.password.length > 8) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'Password cannot exceed 8 characters'
      }, { status: 400 });
    }

    // Check if user already exists
    const normalizedUserId = body.userId.toUpperCase();
    const existingUser = mockUsers.find(u => u.userId === normalizedUserId);
    
    if (existingUser) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_ALREADY_EXISTS',
        errorMessage: 'User ID already exist...'
      }, { status: 409 });
    }

    // Create new user
    const newUser: UserSecurityDTO = {
      userId: normalizedUserId,
      userName: body.userName,
      userType: body.userType,
      email: body.email,
      firstName: body.firstName,
      lastName: body.lastName,
      isActive: body.isActive ?? true,
      failedAttempts: 0,
      isLocked: false,
      createdDate: new Date(),
      modifiedDate: new Date(),
      createdBy: 'SYSTEM', // Should be current user
      modifiedBy: 'SYSTEM'
    };

    mockUsers.push(newUser);

    return NextResponse.json({
      success: true,
      message: `User ${normalizedUserId} has been added...`,
      data: newUser
    }, { status: 201 });

  } catch (error) {
    console.error('Create user error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Unable to Add User...'
    }, { status: 500 });
  }
}