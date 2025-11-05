import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/widgets/category/:category - Filter by category
export async function GET(
  request: NextRequest,
  { params }: { params: { category: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/widgets?category=${params.category}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching widgets by category:', error);
    return NextResponse.json({ error: 'Failed to fetch widgets' }, { status: 500 });
  }
}
