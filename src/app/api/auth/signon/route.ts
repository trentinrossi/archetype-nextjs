// src/api/auth/signon/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, SignonResponseDTO } from '@/types/userSecurity';

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();
    
    // Validate required fields
    if (!body.userId || !body.password) {
      return NextResponse.json({
        success: false,
        errorCode: 'VALIDATION_ERROR',
        errorMessage: body.userId ? 'Please enter Password' : 'Please enter User ID'
      }, { status: 400 });
    }

    // Validate field lengths (COSGN00C business rules)
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

    // Convert userId to uppercase for consistency (COSGN00C business rule)
    const normalizedUserId = body.userId.toUpperCase();

    // Mock authentication logic - replace with actual USRSEC file equivalent
    const mockUsers = [
      { userId: 'ADMIN001', password: 'ADMIN123', userType: 'ADMIN', active: true },
      { userId: 'USER001', password: 'USER123', userType: 'GENERAL', active: true },
      { userId: 'INACTIVE', password: 'PASS123', userType: 'GENERAL', active: false }
    ];

    const user = mockUsers.find(u => u.userId === normalizedUserId);

    if (!user) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_NOT_FOUND',
        errorMessage: 'User not found'
      }, { status: 401 });
    }

    if (!user.active) {
      return NextResponse.json({
        success: false,
        errorCode: 'USER_INACTIVE',
        errorMessage: 'User account is inactive'
      }, { status: 403 });
    }

    if (user.password !== body.password) {
      return NextResponse.json({
        success: false,
        errorCode: 'INVALID_CREDENTIALS',
        errorMessage: 'Wrong Password'
      }, { status: 401 });
    }

    // Successful authentication
    const response: SignonResponseDTO = {
      success: true,
      userId: user.userId,
      userType: user.userType as 'ADMIN' | 'GENERAL',
      sessionToken: `session_${Date.now()}_${user.userId}`,
      expiresAt: new Date(Date.now() + 8 * 60 * 60 * 1000) // 8 hours
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Signon error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    }, { status: 500 });
  }
}