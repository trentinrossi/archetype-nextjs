'use client';

import React, { useEffect, useState } from 'react';
import { useRouter, useParams } from 'next/navigation';
import UserUpdateComponent from '@/components/UserUpdateComponent';
import { userSecurityService } from '@/services/userSecurityService';

const UserEditPage: React.FC = () => {
  const router = useRouter();
  const params = useParams();
  const [isLoading, setIsLoading] = useState<boolean>(true);
  const userId = params.userId as string;

  useEffect(() => {
    // Check authentication and user type
    const session = userSecurityService.getCurrentSession();
    
    if (!session || !userSecurityService.isAuthenticated()) {
      router.push('/');
      return;
    }

    if (session.userType !== 'ADMIN') {
      router.push('/user/dashboard');
      return;
    }

    setIsLoading(false);
  }, [router]);

  const handleUserUpdated = (updatedUserId: string) => {
    console.log('User updated:', updatedUserId);
    // Could show a success notification
  };

  const handleExit = () => {
    router.push('/admin/users');
  };

  if (isLoading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading...</p>
        </div>
      </div>
    );
  }

  return (
    <UserUpdateComponent
      userId={userId}
      onUserUpdated={handleUserUpdated}
      onExit={handleExit}
    />
  );
};

export default UserEditPage;