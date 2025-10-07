// src/app/api/auth/validate/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, ValidationResponseDTO } from '@/types/user-security';

// Mock user database - same as in signon route
const mockUsers = {
  'ADMIN001': {
    userId: 'ADMIN001',
    userName: 'System Administrator',
    userType: 'ADMIN' as const,
    userStatus: 'ACTIVE' as const,
    accountLocked: false
  },
  'USER001': {
    userId: 'USER001',
    userName: 'General User',
    userType: 'USER' as const,
    userStatus: 'ACTIVE' as const,
    accountLocked: false
  }
};

const mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123'
};

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();

    // Validate request body
    if (!body.userId || !body.password) {
      const response: ValidationResponseDTO = {
        isValid: false,
        validationCode: 'MISSING_FIELDS',
        validationMessage: body.userId ? 'Please enter Password' : 'Please enter User ID',
        validationDetails: [
          {
            field: body.userId ? 'password' : 'userId',
            code: 'REQUIRED',
            message: body.userId ? 'Password is required' : 'User ID is required',
            severity: 'ERROR'
          }
        ],
        timestamp: new Date().toISOString()
      };

      return NextResponse.json(response, { status: 400 });
    }

    // Convert userId to uppercase for case-insensitive lookup
    const userId = body.userId.toUpperCase();

    // Check if user exists
    const user = mockUsers[userId as keyof typeof mockUsers];
    if (!user) {
      const response: ValidationResponseDTO = {
        isValid: false,
        validationCode: 'USER_NOT_FOUND',
        validationMessage: 'User not found',
        validationDetails: [
          {
            field: 'userId',
            code: 'NOT_FOUND',
            message: 'The specified user ID does not exist',
            severity: 'ERROR'
          }
        ],
        timestamp: new Date().toISOString()
      };

      return NextResponse.json(response, { status: 401 });
    }

    // Check if account is active
    if (user.userStatus !== 'ACTIVE') {
      const response: ValidationResponseDTO = {
        isValid: false,
        validationCode: 'ACCOUNT_INACTIVE',
        validationMessage: 'User account is inactive',
        validationDetails: [
          {
            field: 'userId',
            code: 'INACTIVE',
            message: 'The user account is not active',
            severity: 'ERROR'
          }
        ],
        timestamp: new Date().toISOString()
      };

      return NextResponse.json(response, { status: 403 });
    }

    // Check if account is locked
    if (user.accountLocked) {
      const response: ValidationResponseDTO = {
        isValid: false,
        validationCode: 'ACCOUNT_LOCKED',
        validationMessage: 'User account is locked',
        validationDetails: [
          {
            field: 'userId',
            code: 'LOCKED',
            message: 'The user account is locked due to security reasons',
            severity: 'ERROR'
          }
        ],
        timestamp: new Date().toISOString()
      };

      return NextResponse.json(response, { status: 403 });
    }

    // Validate password
    const storedPassword = mockPasswords[userId];
    if (!storedPassword || storedPassword !== body.password) {
      const response: ValidationResponseDTO = {
        isValid: false,
        validationCode: 'INVALID_PASSWORD',
        validationMessage: 'Wrong Password',
        validationDetails: [
          {
            field: 'password',
            code: 'INVALID',
            message: 'The provided password is incorrect',
            severity: 'ERROR'
          }
        ],
        timestamp: new Date().toISOString()
      };

      return NextResponse.json(response, { status: 401 });
    }

    // Validation successful
    const response: ValidationResponseDTO = {
      isValid: true,
      validationCode: 'VALID_CREDENTIALS',
      validationMessage: 'Credentials are valid',
      timestamp: new Date().toISOString()
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Validation error:', error);
    
    const response: ValidationResponseDTO = {
      isValid: false,
      validationCode: 'INTERNAL_ERROR',
      validationMessage: 'Internal server error during validation',
      timestamp: new Date().toISOString()
    };

    return NextResponse.json(response, { status: 500 });
  }
}