import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/statements - Generate statements for all accounts
export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/statements', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error generating statements:', error);
    return NextResponse.json({ error: 'Failed to generate statements' }, { status: 500 });
  }
}
