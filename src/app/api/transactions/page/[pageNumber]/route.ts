import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { pageNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/page/${params.pageNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching transaction page:', error);
    return NextResponse.json({ error: 'Failed to fetch transaction page' }, { status: 500 });
  }
}
