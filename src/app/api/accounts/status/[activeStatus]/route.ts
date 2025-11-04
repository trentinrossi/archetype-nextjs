import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts/status/:activeStatus - Get accounts by status
export async function GET(
  request: NextRequest,
  { params }: { params: { activeStatus: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/accounts/status/${params.activeStatus}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching accounts by status:', error);
    return NextResponse.json({ error: 'Failed to fetch accounts by status' }, { status: 500 });
  }
}
