import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { startTransactionId: string } }
) {
  try {
    const { searchParams } = new URL(request.url);
    const pageNumber = searchParams.get('pageNumber') || '1';
    
    const response = await forwardAuthRequest(
      `/api/transactions/search/${params.startTransactionId}?pageNumber=${pageNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error searching transactions:', error);
    return NextResponse.json({ error: 'Failed to search transactions' }, { status: 500 });
  }
}
