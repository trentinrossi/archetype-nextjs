import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// POST /api/accounts/process-payment - Process bill payment
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/accounts/process-payment', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error processing payment:', error);
    return NextResponse.json({ error: 'Failed to process payment' }, { status: 500 });
  }
}
