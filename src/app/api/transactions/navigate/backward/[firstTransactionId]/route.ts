import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { firstTransactionId: string } }
) {
  try {
    const { searchParams } = new URL(request.url);
    const currentPage = searchParams.get('currentPage') || '1';
    
    const response = await forwardAuthRequest(
      `/api/transactions/navigate/backward/${params.firstTransactionId}?currentPage=${currentPage}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error navigating backward:', error);
    return NextResponse.json({ error: 'Failed to navigate backward' }, { status: 500 });
  }
}
