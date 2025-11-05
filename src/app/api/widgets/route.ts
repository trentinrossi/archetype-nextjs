import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/widgets - List all widgets
export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/v1/widgets', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching widgets:', error);
    return NextResponse.json({ error: 'Failed to fetch widgets' }, { status: 500 });
  }
}

// POST /api/widgets - Create a new widget
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/v1/widgets', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating widget:', error);
    return NextResponse.json({ error: 'Failed to create widget' }, { status: 500 });
  }
}
