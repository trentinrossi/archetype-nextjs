// src/api/auth/validate/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, ValidationResponseDTO } from '@/types/userSecurity';

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();
    
    // Validate required fields
    if (!body.userId || !body.password) {
      const response: ValidationResponseDTO = {
        isValid: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: body.userId ? 'Please enter Password' : 'Please enter User ID'
      };
      return NextResponse.json(response, { status: 400 });
    }

    // Validate field lengths
    if (body.userId.length > 8) {
      const response: ValidationResponseDTO = {
        isValid: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'User ID cannot exceed 8 characters'
      };
      return NextResponse.json(response, { status: 400 });
    }

    if (body.password.length > 8) {
      const response: ValidationResponseDTO = {
        isValid: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: 'Password cannot exceed 8 characters'
      };
      return NextResponse.json(response, { status: 400 });
    }

    // Convert userId to uppercase for consistency
    const normalizedUserId = body.userId.toUpperCase();

    // Mock validation logic - replace with actual USRSEC file equivalent
    const mockUsers = [
      { userId: 'ADMIN001', password: 'ADMIN123', userType: 'ADMIN', active: true },
      { userId: 'USER001', password: 'USER123', userType: 'GENERAL', active: true },
      { userId: 'INACTIVE', password: 'PASS123', userType: 'GENERAL', active: false }
    ];

    const user = mockUsers.find(u => u.userId === normalizedUserId);

    if (!user) {
      const response: ValidationResponseDTO = {
        isValid: false,
        errorCode: 'USER_NOT_FOUND',
        errorMessage: 'User not found'
      };
      return NextResponse.json(response, { status: 401 });
    }

    if (!user.active) {
      const response: ValidationResponseDTO = {
        isValid: false,
        errorCode: 'USER_INACTIVE',
        errorMessage: 'User account is inactive'
      };
      return NextResponse.json(response, { status: 401 });
    }

    if (user.password !== body.password) {
      const response: ValidationResponseDTO = {
        isValid: false,
        errorCode: 'INVALID_CREDENTIALS',
        errorMessage: 'Invalid credentials'
      };
      return NextResponse.json(response, { status: 401 });
    }

    // Valid credentials
    const response: ValidationResponseDTO = {
      isValid: true
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Validation error:', error);
    const response: ValidationResponseDTO = {
      isValid: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    };
    return NextResponse.json(response, { status: 500 });
  }
}