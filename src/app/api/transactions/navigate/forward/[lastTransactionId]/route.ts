import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { lastTransactionId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/navigate/forward/${params.lastTransactionId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error navigating forward:', error);
    return NextResponse.json({ error: 'Failed to navigate forward' }, { status: 500 });
  }
}
