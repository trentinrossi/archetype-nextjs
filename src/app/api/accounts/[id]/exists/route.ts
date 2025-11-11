import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts/:id/exists - Check if account exists
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/accounts/${params.id}/exists`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error checking account existence:', error);
    return NextResponse.json(
      { error: 'Failed to check account existence' },
      { status: 500 }
    );
  }
}
