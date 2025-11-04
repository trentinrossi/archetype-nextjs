import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts/group/:groupId - Get accounts by group
export async function GET(
  request: NextRequest,
  { params }: { params: { groupId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/accounts/group/${params.groupId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching accounts by group:', error);
    return NextResponse.json({ error: 'Failed to fetch accounts by group' }, { status: 500 });
  }
}
