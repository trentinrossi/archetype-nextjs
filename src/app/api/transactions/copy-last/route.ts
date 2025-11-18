import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/transactions/copy-last', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error copying last transaction:', error);
    return NextResponse.json({ error: 'Failed to copy last transaction' }, { status: 500 });
  }
}
