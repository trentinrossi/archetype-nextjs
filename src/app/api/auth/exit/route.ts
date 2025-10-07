// src/app/api/auth/exit/route.ts
import { NextRequest, NextResponse } from 'next/server';

// Handle PF3 Exit functionality from COSGN00C business logic
export async function POST(request: NextRequest) {
  try {
    // PF3 key functionality - graceful exit with thank you message
    const response = {
      success: true,
      message: 'Thank you for using the system. Have a great day!',
      timestamp: new Date().toISOString()
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Exit API error:', error);
    
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

// Handle OPTIONS request for CORS
export async function OPTIONS(request: NextRequest) {
  return new NextResponse(null, {
    status: 200,
    headers: {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization',
    },
  });
}