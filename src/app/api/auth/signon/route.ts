// src/app/api/auth/signon/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, SignonResponseDTO, UserSecurityDTO } from '@/types/user-security';

// Mock user database - In production, this would be replaced with actual database calls
const mockUsers: Record<string, UserSecurityDTO> = {
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

// Mock password storage - In production, passwords would be properly hashed
const mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123'
};

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();

    // Validate request body
    if (!body.userId || !body.password) {
      return NextResponse.json(
        {
          success: false,
          message: body.userId ? 'Please enter Password' : 'Please enter User ID',
          timestamp: new Date().toISOString()
        },
        { status: 400 }
      );
    }

    // Convert userId to uppercase for case-insensitive lookup
    const userId = body.userId.toUpperCase();

    // Check if user exists
    const userSecurity = mockUsers[userId];
    if (!userSecurity) {
      return NextResponse.json(
        {
          success: false,
          message: 'User not found',
          timestamp: new Date().toISOString()
        },
        { status: 401 }
      );
    }

    // Check if account is active
    if (userSecurity.userStatus !== 'ACTIVE') {
      return NextResponse.json(
        {
          success: false,
          message: 'User account is inactive',
          timestamp: new Date().toISOString()
        },
        { status: 403 }
      );
    }

    // Check if account is locked
    if (userSecurity.accountLocked) {
      return NextResponse.json(
        {
          success: false,
          message: 'User account is locked',
          timestamp: new Date().toISOString()
        },
        { status: 403 }
      );
    }

    // Validate password
    const storedPassword = mockPasswords[userId];
    if (!storedPassword || storedPassword !== body.password) {
      // In production, increment failed attempts and potentially lock account
      return NextResponse.json(
        {
          success: false,
          message: 'Wrong Password',
          timestamp: new Date().toISOString()
        },
        { status: 401 }
      );
    }

    // Generate session information
    const sessionId = `SESSION_${userId}_${Date.now()}`;
    const sessionToken = `TOKEN_${userId}_${Date.now()}`;
    const expiresIn = 3600; // 1 hour
    const expiryTime = new Date(Date.now() + expiresIn * 1000).toISOString();

    // Update last signon information
    const updatedUserSecurity: UserSecurityDTO = {
      ...userSecurity,
      lastSignonDate: new Date().toISOString().split('T')[0],
      lastSignonTime: new Date().toTimeString().split(' ')[0],
      failedSignonAttempts: 0 // Reset failed attempts on successful login
    };

    const response: SignonResponseDTO = {
      success: true,
      userId: userId,
      userName: userSecurity.userName,
      sessionId: sessionId,
      sessionToken: sessionToken,
      tokenType: 'Bearer',
      expiresIn: expiresIn,
      expiryTime: expiryTime,
      userSecurity: updatedUserSecurity,
      passwordChangeRequired: userSecurity.passwordExpired,
      firstTimeLogin: !userSecurity.lastSignonDate,
      lastSignonDate: userSecurity.lastSignonDate,
      lastSignonTime: userSecurity.lastSignonTime,
      message: 'Authentication successful'
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Signon error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}