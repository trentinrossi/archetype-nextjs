import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/customers/lastname/:lastName - Get customers by last name
export async function GET(
  request: NextRequest,
  { params }: { params: { lastName: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/customers/lastname/${params.lastName}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching customers by last name:', error);
    return NextResponse.json({ error: 'Failed to fetch customers by last name' }, { status: 500 });
  }
}
