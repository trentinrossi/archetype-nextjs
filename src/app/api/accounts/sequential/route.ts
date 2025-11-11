import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts/sequential - Get all accounts sequentially (BR-001)
export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest(
      '/api/accounts/sequential',
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching accounts sequentially:', error);
    return NextResponse.json(
      { error: 'Failed to fetch accounts sequentially' },
      { status: 500 }
    );
  }
}
