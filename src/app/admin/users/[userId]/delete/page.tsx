'use client';

import React, { useEffect, useState } from 'react';
import { useRouter, useParams } from 'next/navigation';
import UserDeleteComponent from '@/components/UserDeleteComponent';
import { userSecurityService } from '@/services/userSecurityService';

const UserDeletePage: React.FC = () => {
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

  const handleUserDeleted = (deletedUserId: string) => {
    console.log('User deleted:', deletedUserId);
    // Redirect back to user list after successful deletion
    setTimeout(() => {
      router.push('/admin/users');
    }, 2000);
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
    <UserDeleteComponent
      userId={userId}
      onUserDeleted={handleUserDeleted}
      onExit={handleExit}
    />
  );
};

export default UserDeletePage;