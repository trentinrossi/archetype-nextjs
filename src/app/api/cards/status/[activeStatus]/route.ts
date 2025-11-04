import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/cards/status/:activeStatus - Get cards by status
export async function GET(
  request: NextRequest,
  { params }: { params: { activeStatus: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/cards/status/${params.activeStatus}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching cards by status:', error);
    return NextResponse.json({ error: 'Failed to fetch cards by status' }, { status: 500 });
  }
}
