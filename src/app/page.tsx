'use client';

import { useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userService } from '@/services/userService';

export default function HomePage() {
  const router = useRouter();

  useEffect(() => {
    const user = userService.getCurrentUser();
    
    if (!user) {
      router.push('/login');
    } else if (user.userType === 'A') {
      router.push('/admin');
    } else {
      router.push('/main');
    }
  }, [router]);

  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 to-indigo-100">
      <div className="text-center">
        <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600 mb-4"></div>
        <p className="text-gray-600">Loading CardDemo...</p>
      </div>
    </div>
  );
}
