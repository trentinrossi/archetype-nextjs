'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import UserListComponent from '@/components/UserListComponent';
import { userSecurityService } from '@/services/userSecurityService';

const UsersListPage: React.FC = () => {
  const router = useRouter();
  const [isLoading, setIsLoading] = useState<boolean>(true);

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

  const handleUserUpdate = (userId: string) => {
    router.push(`/admin/users/${userId}/edit`);
  };

  const handleUserDelete = (userId: string) => {
    router.push(`/admin/users/${userId}/delete`);
  };

  const handleExit = () => {
    router.push('/admin/dashboard');
  };

  if (isLoading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading users...</p>
        </div>
      </div>
    );
  }

  return (
    <UserListComponent
      onUpdate={handleUserUpdate}
      onDelete={handleUserDelete}
      onExit={handleExit}
    />
  );
};

export default UsersListPage;