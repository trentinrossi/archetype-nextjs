import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/users/:userId/accessible-accounts - Get accessible account IDs for user
export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/users/${params.userId}/accessible-accounts`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching accessible accounts:', error);
    return NextResponse.json(
      { error: 'Failed to fetch accessible accounts' },
      { status: 500 }
    );
  }
}
