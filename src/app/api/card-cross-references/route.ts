import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/card-cross-references - List all card cross-references with pagination
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let queryString = `?page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(`/api/card-cross-references${queryString}`, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching card cross-references:', error);
    return NextResponse.json({ error: 'Failed to fetch card cross-references' }, { status: 500 });
  }
}

// POST /api/card-cross-references - Create a new card cross-reference
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/card-cross-references', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating card cross-reference:', error);
    return NextResponse.json({ error: 'Failed to create card cross-reference' }, { status: 500 });
  }
}
